%%% ------------------------------------------------------------------------------------------------
%%% ETCP is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version 17.9.10
%% -------------------------------------------------------------------------------------------------
-module(etcp_acceptor_manager).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

-export([start_link/5
        ,start_link/4
        ,mode/1
        ,suspend/1
        ,resume/1
        ,filter_mode/1
        ,terminate/3]).

%% 'proc_lib' callback:
-export([init/6]).

%% Internal export:
-export([loop/3]).

%% 'sys' callbacks:
-export([handle_debug/3
        ,system_terminate/4
        ,system_replace_state/2
        ,system_get_state/1
        ,system_continue/3
        ,system_code_change/4]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {mode, acceptor}).

-include("internal/etcp_acceptor.hrl").
-define(GEN_CALL_TAG, '$gen_call').
-define(HIBERNATE_TIMEOUT, 10000).
-define(MODE_TAG, 'mode').
-define(RESUME_TAG, 'resume').
-define(SUSPEND_TAG, 'suspend').
-define(NO_PROCESS_KEEPER, 'undefined').

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), term(), etcp:start_options(), etcp:socket(), etcp:process_registry()) ->
    etcp:start_return().
start_link(Mod, InitArg, Opts, TrS, ProcKeeper) when erlang:is_atom(Mod) andalso
                                                     erlang:is_map(Opts)      ->
    proc_lib:start_link(?MODULE, init, [erlang:self(), Mod, InitArg, Opts, TrS, ProcKeeper]).


-spec
start_link(module(), term(), etcp:start_options(), etcp:socket()) ->
    etcp:start_return().
start_link(Mod, InitArg, Opts, TrS) when erlang:is_atom(Mod) andalso
                                         erlang:is_map(Opts)      ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[erlang:self(), Mod, InitArg, Opts, TrS, ?NO_PROCESS_KEEPER]).


mode(AccMngr) when erlang:is_pid(AccMngr) ->
    gen_server:call(AccMngr, ?MODE_TAG).


suspend(AccMngr) when erlang:is_pid(AccMngr) ->
    gen_server:call(AccMngr, ?SUSPEND_TAG).


resume(AccMngr) when erlang:is_pid(AccMngr) ->
    gen_server:call(AccMngr, ?RESUME_TAG).

%% -------------------------------------------------------------------------------------------------
%% 'proc_lib' callback:

%% @hidden
init(Parent, Mod, InitArg, Opts, TrState, ProcKeeper) ->
    Mode = etcp_utils:get_value(acceptor_mode, Opts, ?DEF_ACCEPTOR_MODE, {?MODULE, filter_mode, 1}),
    erlang:process_flag(trap_exit, true),
    Args =
        if
            ProcKeeper == ?NO_PROCESS_KEEPER ->
                [Mod, InitArg, Opts, TrState, Mode];
            true ->
                [Mod, InitArg, Opts, TrState, Mode, ProcKeeper]
        end,
    try erlang:apply(etcp_acceptor, start_link, Args) of
        {ok, Pid} ->
            proc_lib:init_ack(Parent, {ok, erlang:self()}),
            erlang:garbage_collect(),
            loop(Parent, [], #?S{mode = Mode, acceptor = Pid});
        {error, Rsn}=Err ->
            proc_lib:init_ack(Parent, Err),
            erlang:exit(Rsn)
    catch
        _:Rsn -> % system_limit
            Rsn2 = {start, [{reason, Rsn}, {module, etcp_acceptor}]},
            proc_lib:init_ack(Parent, {error, Rsn2}),
            erlang:exit(Rsn2)
    end.

%% -------------------------------------------------------------------------------------------------
%% 'sys' callbacks:

%% @hidden
system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.


%% @hidden
system_continue(Parent, Dbg, State) ->
    ?MODULE:loop(Parent, Dbg, State).


%% @hidden
system_get_state(S) ->
    {ok, S}.


%% @hidden
system_replace_state(StateFun, S) ->
    S2 = StateFun(S),
    {ok, S2, S2}.


%% @hidden
system_terminate(Rsn, _, Dbg, State) ->
    terminate(Dbg, State, Rsn).


handle_debug(IODev, Event, Name) ->
    io:format(IODev, "*DBG* ETCP acceptor ~p got debug event ~p\n", [Name, Event]).

%% -------------------------------------------------------------------------------------------------
%% Internals:

%% @hidden
loop(Parent, Dbg, S) ->
    receive
        Msg ->
            process_message(Msg, Parent, Dbg, S)
    after ?HIBERNATE_TIMEOUT ->
        erlang:hibernate(?MODULE, loop, [Parent, Dbg, S])
    end.


process_message({?GEN_CALL_TAG, From, Request}, Parent, Dbg, State) ->
    {Dbg2, State2} = process_request(Request, debug(Dbg, {call, From, Request}), State, From),
    ?MODULE:loop(Parent, Dbg2, State2);
process_message({'EXIT', Pid, Rsn}, _, Dbg, State) when Pid == State#?S.acceptor ->
    terminate(Dbg, State, Rsn);
process_message({'EXIT', Pid, Rsn}, Parent, Dbg, State) when Pid == Parent ->
    terminate(Dbg, State, Rsn);
process_message({system, From, SysMsg}, Parent, Dbg, State) ->
    sys:handle_system_msg(SysMsg, From, Parent, ?MODULE, Dbg, State);
process_message(_, Parent, Dbg, State) ->
    ?MODULE:loop(Parent, Dbg, State).


process_request(?MODE_TAG, Dbg, State, From) ->
    {reply(Dbg, From, State#?S.mode), State};
process_request(Action, Dbg, State, From) when Action == ?SUSPEND_TAG ->
    etcp_acceptor:suspend(State#?S.acceptor),
    {reply(Dbg, From, ok), State#?S{mode = ?ACCEPTOR_SLEEP_MODE}};
process_request(Action, Dbg, State, From) when Action == ?RESUME_TAG ->
    etcp_acceptor:resume(State#?S.acceptor),
    {reply(Dbg, From, ok), State#?S{mode = ?ACCEPTOR_ACCEPT_MODE}};
process_request(Request, Dbg, State, From) ->
    {reply(Dbg, From, {error, {unknown, [{request, Request}]}}), State}.


terminate(_, _, Rsn) ->
    erlang:exit(Rsn).


debug([], _) ->
    [];
debug(Dbg, Event) ->
    sys:handle_debug(Dbg, fun ?MODULE:handle_debug/3, erlang:self(), Event).


filter_mode(?ACCEPTOR_ACCEPT_MODE) ->
    true;
filter_mode(?ACCEPTOR_SLEEP_MODE) ->
    true;
filter_mode(_) ->
    false.


reply(Dbg, {Pid, Tag}=Client, Msg) ->
    catch Pid ! {Tag, Msg},
    debug(Dbg, {out, Client, Msg});
reply(Dbg, Pid, Msg) ->
    catch Pid ! Msg,
    debug(Dbg, {out, Pid, Msg}).
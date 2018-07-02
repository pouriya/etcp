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
%% @doc
%%
%% @end
%% -------------------------------------------------------------------------------------------------
-module(etcp_acceptor_sup).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/4
        ,start_link/5
        ,fetch/1
        ,suspend/1
        ,resume/1
        ,modes/1
        ,start_acceptor/2]).

%% 'director' callbacks:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {}).

-define(NO_PROCESS_KEEPER, 'undefined').

-define(DEF_START_OPTS, []).
-include("internal/etcp_guard.hrl").
-include("internal/etcp_acceptor.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), term(), etcp:start_options(), etcp:socket()) ->
    etcp:start_return().
start_link(Mod, InitArg, Opts, TrS) when erlang:is_map(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts, TrS, ?NO_PROCESS_KEEPER}, ?DEF_START_OPTS).


-spec
start_link(module(), term(), etcp:start_options(), etcp:socket(), etcp:process_registry()) ->
    etcp:start_return().
start_link(Mod, InitArg, Opts, TrS, ProcKeeper) when erlang:is_map(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts, TrS, ProcKeeper}, ?DEF_START_OPTS).


-spec
fetch(etcp:name()) ->
    [] | [{etcp:acceptor_id(), pid()}].
%% @doc
%%      returns all acceptor managers.
%% @end
fetch(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, Pids} = director:get_pids(AccRootSup),
    Pids.


-spec
suspend(etcp:name()) ->
    'ok'.
suspend(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    SuspendFun =
        fun({_, AccPid}) ->
            etcp_acceptor_manager:suspend(AccPid)
        end,
    lists:foreach(SuspendFun, AccSups).


-spec
resume(etcp:name()) ->
    'ok'.
resume(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    ResumeFun =
        fun({_, AccPid}) ->
            etcp_acceptor_manager:resume(AccPid)
        end,
    lists:foreach(ResumeFun, AccSups).


-spec
modes(etcp:name()) ->
    etcp:acceptor_mode() | [etcp:acceptor_mode()].
modes(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    Modes = [{Id, etcp_acceptor_manager:mode(AccSupPid)} || {Id, AccSupPid} <- AccSups],
    case Modes of
        [{_, Mode}, _ | _] ->
            FilterFun =
                fun({_, Mode_}) ->
                    if
                        Mode == Mode_ ->
                            true;
                        true ->
                            false
                    end
                end,
            case lists:all(FilterFun, Modes) of
                true ->
                    Mode;
                _ -> % false
                    Modes
            end;
        [{_, Mode}] ->
            Mode
    end.


%%-spec
%%start_acceptor(etcp:name(), term(), etcp:name()) ->
%%    'ok' | etcp:error().
%%start_acceptor(AccRootSup, Id, ProcKeeper) when erlang:is_pid(AccRootSup) ->
%%    case director:start_child(AccRootSup
%%                             ,#{start => {etcp_acceptor_manager, start_link, [ProcKeeper]}
%%                               ,id => Id
%%                               ,append => true}) of
%%        {ok, _} ->
%%            ok;
%%        Err -> % {error, _}
%%            Err
%%    end.


-spec
start_acceptor(etcp:name(), term()) ->
    'ok' | etcp:error().
start_acceptor(AccRootSup, Id) when erlang:is_pid(AccRootSup) ->
    case director:start_child(AccRootSup, #{start => {etcp_acceptor_manager, start_link, []}
                                           ,id => Id
                                           ,append => true}) of
        {ok, _} ->
            ok;
        Err -> % {error, _}
            Err
    end.

%% -------------------------------------------------------------------------------------------------
%% 'director' callbacks:

%% @hidden
init({Mod, InitArg, Opts, TrS, ProcKeeper}) ->
    Args =
        if
            ProcKeeper == ?NO_PROCESS_KEEPER ->
                [Mod, InitArg, Opts, TrS];
            true -> % {apply, _} | Pid
                [Mod, InitArg, Opts, TrS, ProcKeeper]
    end,
    DefChildSpec = #{start => {etcp_acceptor_manager, start_link, Args}},
    {ok, #?S{}, [], DefChildSpec}.


%% @hidden
handle_start(_, ChildState, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
handle_exit(_, ChildState, _, State, _) ->
    {stop, ChildState, State, [{log, false}]}.


%% @hidden
handle_terminate(_, ChildState, _, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
terminate(_, _) ->
    {ok, [{log, false}]}.
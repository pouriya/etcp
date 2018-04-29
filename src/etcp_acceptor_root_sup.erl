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
-module(etcp_acceptor_root_sup).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/4
        ,fetch/1
        ,sleep/1
        ,accept/1
        ,modes/1
        ,start_acceptor_sup/1
        ,start_acceptor_sup/2]).

%% 'director' callbacks:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {errors}).

-define(DEF_START_OPTS, []).
-define(DEF_ACCEPTOR_COUNT, 3).
-include("internal/etcp_guard.hrl").
-include("internal/etcp_acceptor.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), term(), etcp_types:start_options(), etcp_types:socket()) ->
    etcp_types:start_return().
start_link(Mod, InitArg, Opts, LSock) when erlang:is_map(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts, LSock}, ?DEF_START_OPTS).


-spec
fetch(etcp_types:name()) ->
    [] | [{reference(), pid()}].
%% @doc
%%      returns all acceptors.
%% @end
fetch(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, Pids} = director:get_pids(AccRootSup),
    Fold =
        fun
            ({ok, AccInfo}, Acc) ->
                [AccInfo|Acc];
            (_, Acc) ->
                Acc
        end,
    lists:foldl(Fold, [], [etcp_acceptor_sup:fetch(AccSupPid) || {_, AccSupPid} <- Pids]).


-spec
sleep(etcp_types:name()) ->
    'ok'.
sleep(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    ForEach =
        fun({_, AccPid}) ->
            etcp_acceptor_sup:sleep(AccPid)
        end,
    lists:foreach(ForEach, AccSups).


-spec
accept(etcp_types:name()) ->
    'ok'.
accept(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    ForEach =
        fun({_, AccPid}) ->
            etcp_acceptor_sup:accept(AccPid)
        end,
    lists:foreach(ForEach, AccSups).


-spec
modes(etcp_types:name()) ->
    etcp_types:acceptor_mode() | [etcp_types:acceptor_mode()].
modes(AccRootSup) when erlang:is_pid(AccRootSup) ->
    {ok, AccSups} = director:get_pids(AccRootSup),
    Modes = [Mode
            || Mode <- [etcp_acceptor_sup:mode(AccSupPid) || {_, AccSupPid} <- AccSups]
            ,Mode == ?ACCEPTOR_ACCEPT_MODE orelse Mode == ?ACCEPTOR_SLEEP_MODE],
    case Modes of
        [Mode|_] ->
            All =
                fun(Mode_) ->
                    if
                        Mode == Mode_ ->
                            true;
                        true ->
                            false
                    end
                end,
            case lists:all(All, Modes) of
                true ->
                    Mode;
                false ->
                    Modes
            end;
        _ ->
            Modes
    end.


-spec
start_acceptor_sup(etcp_types:name(), etcp_types:name()) ->
    'ok' | etcp_types:error().
start_acceptor_sup(AccRootSup, ProcReg) when erlang:is_pid(AccRootSup) andalso
                                             ?is_process_registry(ProcReg) ->
    case director:start_child(AccRootSup, #{start => {etcp_acceptor_sup, start_link, [ProcReg]}
                                           ,id => erlang:make_ref()
                                           ,append => true}) of
        {ok, _} ->
            ok;
        Err -> % {error, _}
            Err
    end.


-spec
start_acceptor_sup(etcp_types:name()) ->
    'ok' | etcp_types:error().
start_acceptor_sup(AccRootSup) when erlang:is_pid(AccRootSup) ->
    start_acceptor_sup(AccRootSup, undefined).

%% -------------------------------------------------------------------------------------------------
%% 'director' callbacks:

init({Mod, InitArg, Opts, LSock}) ->
    DefChildSpec = #{start => {etcp_acceptor_sup, start_link, [Mod, InitArg, Opts, LSock]}
                    ,type => sup},
    {ok, #?S{errors = []}, [], DefChildSpec}.


%% @hidden
handle_start(_, ChildState, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
handle_exit(_, ChildState, Rsn, #?S{errors = Errs}=State, _) ->
    {State2, LogFlag} =
        case lists:member(Rsn, Errs) of
            true ->
                {State, false};
            _ ->
                LogFlag2 =
                    if
                        Rsn =:= normal orelse Rsn =:= shutdown ->
                            false;
                        true ->
                            true
                    end,
                {State#?S{errors = [Rsn|Errs]}, LogFlag2}
        end,
    {stop, ChildState, State2, [{log, LogFlag}]}.


%% @hidden
handle_terminate(_, ChildState, Rsn, #?S{errors = Errs}=State, _) ->
    {State2, LogFlag} =
        case lists:member(Rsn, Errs) of
            true ->
                {State, false};
            _ ->
                LogFlag2 =
                    if
                        Rsn =:= normal orelse Rsn =:= shutdown ->
                            false;
                        true ->
                            true
                    end,
                {State#?S{errors = [Rsn|Errs]}, LogFlag2}
        end,
    {ok, ChildState, State2, [{log, LogFlag}]}.


%% @hidden
terminate(Rsn, #?S{errors = Errs}) ->
    Rsn2 =
        case Errs of
            [Rsn3] ->
                Rsn3;
            [] ->
                Rsn;
            _ ->
                Errs
        end,
    LogFlag =
        if
            Rsn2 =:= normal orelse Rsn2 =:= shutdown ->
                false;
            true ->
                true
        end,
    {new_error, Rsn2, [{log, LogFlag}]}.
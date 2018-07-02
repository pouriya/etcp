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
%% @hidden
%% -------------------------------------------------------------------------------------------------
-module(etcp_acceptor_sup2).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/5
        ,fetch/1
        ,sleep/1
        ,accept/1
        ,mode/1
        ,start_acceptor/1]).

%% 'director' callback:
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

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API functions:

-spec
start_link(module(), term(), etcp_types:start_options(), etcp_types:socket(), pid()) ->
    etcp_types:start_return().
start_link(Mod, InitArg, Opts, LSock, ProcReg) when erlang:is_map(Opts) andalso
                                                    ?is_process_registry(ProcReg) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts, LSock, ProcReg}, ?DEF_START_OPTS).


-spec
fetch(etcp_types:name()) ->
    {'ok', {reference(), pid()}} | {'error', 'not_found'}.
%% @doc
%%      returns all acceptors.
%% @end
fetch(AccSup) when erlang:is_pid(AccSup) ->
    case director:get_pids(AccSup) of
        {ok, [Acc]} ->
            {ok, Acc};
        {ok, []} ->
            {error, not_found}
    end.


-spec
sleep(etcp_types:name()) ->
    'ok' | {'error', 'not_found'}.
sleep(AccSup) when erlang:is_pid(AccSup) ->
    case fetch(AccSup) of
        {ok, {_, Pid}} ->
            etcp_acceptor_manager:sleep(Pid);
        {error, not_found}=Err ->
            Err
    end.


-spec
accept(etcp_types:name()) ->
    'ok' | {'error', 'not_found'}.
accept(AccSup) when erlang:is_pid(AccSup) ->
    case fetch(AccSup) of
        {ok, {_, Pid}} ->
            etcp_acceptor_manager:accept(Pid);
        {error, not_found}=Err ->
            Err
    end.


-spec
mode(etcp_types:name()) ->
    etcp_types:acceptor_mode() | {'error', 'not_found'}.
mode(AccSup) when erlang:is_pid(AccSup) ->
    case fetch(AccSup) of
        {ok, {_, Pid}} ->
            etcp_acceptor_manager:mode(Pid);
        {error, not_found}=Err ->
            Err
    end.


-spec
start_acceptor(etcp_types:name()) ->
    'ok' | etcp_types:error().
start_acceptor(AccSup) when erlang:is_pid(AccSup) ->
    case director:start_child(AccSup, #{start => {etcp_connection, start_link, []}
                                       ,id => erlang:make_ref()
                                       ,append => true}) of
        {ok, _} ->
            ok;
        {error, _}=Err ->
            Err
    end.

%% -------------------------------------------------------------------------------------------------
%% 'director' callbacks:

%% @hidden
init({Mod, InitArg, Opts, LSock, ProcReg}) ->
    AcceptorChildSpec = #{start => {etcp_acceptor
                                   ,start_link
                                   ,[Mod, InitArg, Opts, LSock, ProcReg]}
                         ,id => etcp_acceptor},
    {ok, #?S{errors = []}, [AcceptorChildSpec]}.


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
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
-module(etcp_server).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/3
        ,start_link/4
        ,start_link/5

        ,connections/1
        ,acceptors/1

        ,sleep/1
        ,accept/1
        ,modes/1

        ,stop/1
        ,stop/2]).

%% 'director' export:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Record & Macros & Includes:

-define(S, state).
-record(?S, {transporter_module, transporter_state, listen_socket, errors}).

-define(ACCEPTOR_ROOT_SUP, 'etcp_acceptor_root_sup').
-define(PROCESS_REGISTRY, 'etcp_server_process_registry').
-define(DEF_DIRECTOR_START_OPTS, []).
-define(DEF_START_OPTS, #{}).
-define(DEF_ACCEPTOR_COUNT, 10).
-define(DEF_TRANSPORT_OPTIONS, []).
-define(DEF_TERMINATE_TIMEOUT, 'infinity').
-define(DEF_TRANSPORT_MODULE, 'etcp_transporter_tcp').
-define(DEF_CONNECTION_PROCESS_REGISTRY, 'true').

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API functions:

-spec
start_link(module(), etcp_types:init_argument(), etcp_types:port_number()) ->
     etcp_types:start_return().
start_link(Mod, InitArg, Port) when erlang:is_atom(Mod),
                                    erlang:is_integer(Port) ->
    start_link(Mod, InitArg, Port, ?DEF_START_OPTS).


-spec
start_link(etcp_types:register_name() | module()
          ,module() | etcp_types:init_argument()
          ,etcp_types:init_argument() | etcp_types:port_number()
          ,etcp_types:port_number() | etcp_types:start_options()) ->
    etcp_types:start_return().
start_link(Mod, InitArg, Port, Opts) when erlang:is_atom(Mod),
                                          erlang:is_integer(Port),
                                          erlang:is_map(Opts) ->
    case director:start_link(?MODULE, {Mod, InitArg, Port, Opts}, ?DEF_DIRECTOR_START_OPTS) of
        {ok, Pid} ->
            AcceptorCount = etcp_utils:get_value(acceptor_count
                                                ,Opts
                                                ,?DEF_ACCEPTOR_COUNT
                                                ,fun etcp_utils:is_whole_integer/1),
            continue_starting(Pid, AcceptorCount);
        {error, _}=Err ->
            Err;
        ignore ->
            ignore
    end;
start_link(Name, Mod, InitArg, Port) when erlang:is_tuple(Name),
                                          erlang:is_atom(Mod),
                                          erlang:is_integer(Port) ->
    case director:start_link(Name
                            ,?MODULE
                            ,{Mod, InitArg, Port, ?DEF_START_OPTS}
                            ,?DEF_DIRECTOR_START_OPTS) of
        {ok, Pid} ->
            continue_starting(Pid, ?DEF_ACCEPTOR_COUNT);
        {error, _}=Err ->
            Err;
        ignore ->
            ignore
    end.


-spec
start_link(etcp_types:register_name()
          ,module()
          ,etcp_types:init_argument()
          ,etcp_types:port_number()
          ,etcp_types:start_options()) ->
    etcp_types:start_return().
start_link(Name, Mod, InitArg, Port, Opts) when erlang:is_tuple(Name),
                                                erlang:is_atom(Mod),
                                                erlang:is_integer(Port),
                                                erlang:is_map(Opts) ->
    case director:start_link(Name, ?MODULE, {Mod, InitArg, Port, Opts}, ?DEF_DIRECTOR_START_OPTS) of
        {ok, Pid} ->
            AcceptorCount = etcp_utils:get_value(acceptor_count
                                                ,Opts
                                                ,?DEF_ACCEPTOR_COUNT
                                                ,fun etcp_utils:is_whole_integer/1),
            continue_starting(Pid, AcceptorCount);
        {error, _}=Err ->
            Err;
        ignore ->
            ignore
    end.


-spec
acceptors(etcp_types:name()) ->
    [{reference(), pid()}].
acceptors(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_root_sup:fetch(Pid).


-spec
connections(etcp_types:name()) ->
    [] | [{reference(), pid()}] | 'no_process_registry'.
connections(Server) when ?is_proc_ref(Server) ->
    case director:get_pid(Server, ?PROCESS_REGISTRY) of
        {ok, Pid} ->
            etcp_server_process_registry:fetch(Pid);
        _ -> % {error, not_found}
            no_process_registry
    end.


-spec
sleep(etcp_types:name()) ->
    'ok'.
sleep(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_root_sup:sleep(Pid).


-spec
accept(etcp_types:name()) ->
    'ok'.
accept(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_root_sup:accept(Pid).


-spec
modes(etcp_types:name()) ->
    etcp_types:acceptor_mode() | [etcp_types:acceptor_mode()].
modes(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_root_sup:modes(Pid).


-spec
stop(etcp_types:name()) ->
    'ok'.
stop(Server) when ?is_proc_ref(Server) ->
    director:stop(Server, normal, ?DEF_TERMINATE_TIMEOUT).


-spec
stop(etcp_types:name(), Reason:: etcp_types:reason()) ->
    'ok'.
stop(Server, Reason) when ?is_proc_ref(Server) ->
    director:stop(Server, Reason, ?DEF_TERMINATE_TIMEOUT).

%% -------------------------------------------------------------------------------------------------
%% 'director' callback:

%% @hidden
init({Mod, InitArg, Port, Opts}) ->
    TrMod = etcp_utils:get_value(transporter
                                ,Opts
                                ,?DEF_TRANSPORT_MODULE
                                ,fun erlang:is_atom/1),
    TrOpts = etcp_utils:get_value(transporter_options
                                 ,Opts
                                 ,?DEF_TRANSPORT_OPTIONS
                                 ,fun(_) -> true end),
    case etcp_transporter:init(TrMod, TrOpts) of
        {ok, TrState} ->
            ProcRegFlag = etcp_utils:get_value(connection_process_regisrty
                                              ,Opts
                                              ,?DEF_CONNECTION_PROCESS_REGISTRY
                                              ,fun erlang:is_boolean/1),
            case etcp_transporter:listen(TrMod, Port, TrState) of
                {ok, {LSock, TrState2}} ->
                    OKRet =
                        fun(InitArgX) ->
                            State = #?S{listen_socket = LSock
                                       ,transporter_module = TrMod
                                       ,transporter_state = TrState2
                                       ,errors = []},
                            AccRootSupChildSpec = #{id => ?ACCEPTOR_ROOT_SUP
                                                  ,start => {etcp_acceptor_root_sup
                                                            ,start_link
                                                            ,[Mod, InitArgX, Opts, LSock]}},
                            Children =
                                if
                                    ProcRegFlag ->
                                        ProcRegChildSpec = #{id => ?PROCESS_REGISTRY
                                                           ,start => {etcp_server_process_registry
                                                                     ,start_link
                                                                     ,[]}
                                                           ,type => sup},
                                        [ProcRegChildSpec, AccRootSupChildSpec];
                                    true ->
                                        [AccRootSupChildSpec]
                                end,
                            {ok, State, Children}
                        end,
                    try Mod:listen_init(InitArg, Opts, LSock) of
                        ok ->
                            OKRet(InitArg);
                        {ok, InitArg2} ->
                            OKRet(InitArg2);
                        ignore ->
                            ignore;
                        {stop, _}=Stop ->
                            Stop;
                        Other ->
                            {stop, {return, [{value, Other}
                                            ,{module, Mod}
                                            ,{function, listen_init}
                                            ,{arguments, [InitArg, Opts, LSock]}]}}
                    catch
                        _:Rsn ->
                            {stop, {crash, [{reason, Rsn}
                                           ,{stacktrace, erlang:get_stacktrace()}
                                           ,{module, Mod}
                                           ,{function, listen_init}
                                           ,{arguments, [InitArg, Opts, LSock]}]}}
                    end;
                {error, Rsn} ->
                    {stop, Rsn}
            end;
        {error, Rsn} ->
            {stop, Rsn}
    end.



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
terminate(Rsn
         ,#?S{listen_socket = LSock
             ,transporter_module = TrMod
             ,transporter_state = TrState
             ,errors = Errs}) ->
    _ = etcp_transporter:close(TrMod, LSock, TrState),
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

%% -------------------------------------------------------------------------------------------------
%% Internal functions:


continue_starting(Pid, AcceptorCount) ->
    {ok, AccRootSup} = director:get_pid(Pid, ?ACCEPTOR_ROOT_SUP),
    AddFun =
        case director:get_pid(Pid, ?PROCESS_REGISTRY) of
            {ok, ProcReg} ->
                fun(_) ->
                    etcp_acceptor_root_sup:start_acceptor_sup(AccRootSup, ProcReg)
                end;
            _ -> % {error, not_found}
                fun(_) ->
                    etcp_acceptor_root_sup:start_acceptor_sup(AccRootSup)
                end
        end,
    _ = lists:foreach(AddFun, lists:seq(1, AcceptorCount)),
    {ok, Pid}.
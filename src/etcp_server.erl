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
        ,suspend/1
        ,resume/1
        ,modes/1
        ,stop/1
        ,stop/2]).

%% 'director' export:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% 'etcp_config' callback:
-export([filter_connection_keeper/1]).

%% -------------------------------------------------------------------------------------------------
%% Record & Macros & Includes:

-define(S, state).
-record(?S, {transporter_state}).

-define(ACCEPTOR_ROOT_SUP, 'etcp_acceptor_sup').
-define(PROCESS_KEEPER, 'process_keeper').
-define(DEF_DIRECTOR_START_OPTS, []).
-define(DEF_START_OPTS, #{}).
-define(DEF_ACCEPTOR_COUNT, 10).
-define(DEF_TRANSPORTER_OPTIONS, []).
-define(DEF_TRANSPORTER, 'tcp').
-define(DEF_CONNECTION_PROCESS_KEEPER, {'child', {etcp_server_process_keeper}}).

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API functions:

-spec
start_link(module(), etcp:init_argument(), etcp:port_number()) ->
     etcp:start_return().
start_link(Mod, InitArg, Port) when erlang:is_atom(Mod) andalso erlang:is_integer(Port) ->
    start_link(Mod, InitArg, Port, ?DEF_START_OPTS).


-spec
start_link(etcp:register_name() | module()
          ,module()             | etcp:init_argument()
          ,etcp:init_argument() | etcp:port_number()
          ,etcp:port_number()   | etcp:start_options()) ->
    etcp:start_return().
start_link(Mod, InitArg, Port, Opts) when erlang:is_atom(Mod)     andalso
                                          erlang:is_integer(Port) andalso
                                          erlang:is_map(Opts)          ->
    AcceptorCount = etcp_config:value(acceptor_count
                                     ,Opts
                                     ,?DEF_ACCEPTOR_COUNT
                                     ,{etcp_utils, is_whole_integer, 1}),
    case director:start_link(?MODULE, {Mod, InitArg, Port, Opts}, ?DEF_DIRECTOR_START_OPTS) of
        {ok, Pid} ->
            continue_starting(Pid, AcceptorCount);
        {error, _}=Err ->
            Err;
        ignore ->
            ignore
    end;
start_link(Name, Mod, InitArg, Port) when erlang:is_tuple(Name) andalso
                                          erlang:is_atom(Mod)   andalso
                                          erlang:is_integer(Port)    ->
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
start_link(etcp:register_name()
          ,module()
          ,etcp:init_argument()
          ,etcp:port_number()
          ,etcp:start_options()) ->
    etcp:start_return().
start_link(Name, Mod, InitArg, Port, Opts) when erlang:is_tuple(Name)   andalso
                                                erlang:is_atom(Mod)     andalso
                                                erlang:is_integer(Port) andalso
                                                erlang:is_map(Opts)          ->
    AcceptorCount = etcp_config:value(acceptor_count
                                     ,Opts
                                     ,?DEF_ACCEPTOR_COUNT
                                     ,{etcp_utils, is_whole_integer, 1}),
    case director:start_link(Name, ?MODULE, {Mod, InitArg, Port, Opts}, ?DEF_DIRECTOR_START_OPTS) of
        {ok, Pid} ->
            continue_starting(Pid, AcceptorCount);
        {error, _}=Err ->
            Err;
        ignore ->
            ignore
    end.


-spec
acceptors(etcp:name()) ->
    [{etcp:acceptor_id(), pid()}].
acceptors(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_sup:fetch(Pid).


-spec
connections(etcp:name()) ->
    [] | [pid()] | 'no_process_keeper'.
connections(Server) when ?is_proc_ref(Server) ->
    case director:get_pid(Server, ?PROCESS_KEEPER) of
        {ok, Pid} ->
            etcp_server_process_keeper:fetch(Pid);
        _ -> % {error, not_found}
            no_process_keeper
    end.


-spec
suspend(etcp:name()) ->
    'ok'.
suspend(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_sup:suspend(Pid).


-spec
resume(etcp:name()) ->
    'ok'.
resume(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_sup:resume(Pid).


-spec
modes(etcp:name()) ->
    etcp:acceptor_mode() | [{etcp:acceptor_id(), etcp:acceptor_mode()}].
modes(Server) when ?is_proc_ref(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_ROOT_SUP),
    etcp_acceptor_sup:modes(Pid).


-spec
stop(etcp:name()) ->
    'ok'.
stop(Server) when ?is_proc_ref(Server) ->
    director:stop(Server, normal, infinity).


-spec
stop(etcp:name(), Reason:: etcp:reason()) ->
    'ok'.
stop(Server, Reason) when ?is_proc_ref(Server) ->
    director:stop(Server, Reason, infinity).

%% -------------------------------------------------------------------------------------------------
%% 'director' callback:

%% @hidden
init({Mod, InitArg, Port, Opts}) ->
    TrMod   = etcp_config:value(transporter
                               ,Opts
                               ,?DEF_TRANSPORTER
                               ,{etcp_transporter, filter_transporter, 1}),
    TrOpts  = etcp_config:value(transporter_options, Opts, ?DEF_TRANSPORTER_OPTIONS),
    ProcReg = etcp_config:value(connection_keeper
                               ,Opts
                               ,?DEF_CONNECTION_PROCESS_KEEPER
                               ,{?MODULE, filter_connection_keeper, 1}),
    case etcp_transporter:init(TrMod, TrOpts) of
        {ok, TrState} ->
            case etcp_transporter:listen(Port, TrState) of
                {ok, TrState2} ->
                    try Mod:etcp_server_init(InitArg) of
                        {ok, InitArg2} ->
                            Children =
                                case ProcReg of
                                    {child, ProcRegStart} ->
                                        [#{id => ?PROCESS_KEEPER
                                          ,start => ProcRegStart
                                          ,type => sup}
                                        ,#{id => ?ACCEPTOR_ROOT_SUP
                                          ,start => {etcp_acceptor_sup
                                                    ,start_link
                                                    ,[Mod, InitArg2, Opts, TrState2]}
                                          ,type => sup}];
                                    false ->
                                        [#{id => ?ACCEPTOR_ROOT_SUP
                                          ,start => {etcp_acceptor_sup
                                                    ,start_link
                                                    ,[Mod, InitArg2, Opts, TrState2]}
                                          ,type => sup}];
                                    _ -> % {apply, _}
                                        [#{id => ?ACCEPTOR_ROOT_SUP
                                          ,start => {etcp_acceptor_sup
                                                    ,start_link
                                                    ,[Mod, InitArg2, Opts, TrState2, ProcReg]}
                                          ,type => sup}]
                                end,
                            {ok, #?S{transporter_state = TrState2}, Children};
                        ignore ->
                            _ = etcp_transporter:close(TrState2),
                            ignore;
                        {stop, _}=Stop ->
                            _ = etcp_transporter:close(TrState2),
                            Stop;
                        Other ->
                            _ = etcp_transporter:close(TrState2),
                            {stop, {return, [{value, Other}
                                            ,{module, Mod}
                                            ,{function, etcp_server_init}
                                            ,{arguments, [InitArg]}]}}
                    catch
                        _:Rsn ->
                            _ = etcp_transporter:close(TrState2),
                            {stop, {crash, [{reason, Rsn}
                                           ,{stacktrace, erlang:get_stacktrace()}
                                           ,{module, Mod}
                                           ,{function, etcp_server_init}
                                           ,{arguments, [InitArg]}]}}
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
handle_exit(_, ChildState, _, State, _) ->
    {stop, ChildState, State, [{log, false}]}.


%% @hidden
handle_terminate(_, ChildState, _, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
terminate(Rsn, #?S{transporter_state = TrS}) ->
    _ = etcp_transporter:close(TrS),
    LogFlag =
        if
            % We don't want log if we are stopping normally or our supervisor is stopping us:
            Rsn =:= normal orelse Rsn =:= shutdown orelse erlang:element(1, Rsn) =:= shutdown ->
                false;
            true ->
                true
        end,
    {ok, [{log, LogFlag}]}.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:


continue_starting(Pid, AcceptorCount) ->
    {ok, AccRootSup} = director:get_pid(Pid, ?ACCEPTOR_ROOT_SUP),
    AddFun =
        case director:get_pid(Pid, ?PROCESS_KEEPER) of
            {ok, ProcReg} ->
                fun(Id) ->
                    etcp_acceptor_sup:start_acceptor(AccRootSup, Id, ProcReg)
                end;
            _ -> % {error, not_found} and we did not want a process registry child
                fun(Id) ->
                    etcp_acceptor_sup:start_acceptor(AccRootSup, Id)
                end
        end,
    _ = lists:foreach(AddFun, lists:seq(1, AcceptorCount)), % Acceptor Ids: 1, 2, 3, ...
    {ok, Pid}.


filter_connection_keeper(false) ->
    {ok, false}; % Yield false
filter_connection_keeper(true) ->
    {ok, {child, {etcp_server_process_keeper, start_link, []}}};
filter_connection_keeper({child, {Mod, Func, Args}}) when erlang:is_atom(Mod)  andalso
                                                          erlang:is_atom(Func) andalso
                                                          erlang:is_list(Args)      ->
    true; % Yield Arg
filter_connection_keeper({apply, {Mod, Func}}) when erlang:is_atom(Mod) andalso
                                                    erlang:is_atom(Func)     ->
    true; % Yield Arg
filter_connection_keeper(_) ->
    false.
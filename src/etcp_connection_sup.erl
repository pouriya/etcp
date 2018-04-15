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
-module(etcp_connection_sup).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/3
        ,start_link/4
        ,start_link/5
        ,add/3
        ,add/4
        ,fetch/2
        ,stop/1
        ,stop/2]).

%% 'etcp_server_connection_sup' callback:
-export([start_link_/3]).

%% 'etcp_acceptor' callback:
-export([add_/2]).

%% 'director' callback:
-export([init/1
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(DEF_START_OPTIONS, []).
-define(DEF_TERMINATE_TIMEOUT, 'infinity').
-define(DEF_CHILDSPEC_PLAN, 'undefined').
-define(DEF_CHILDSPEC_ERROR_LOGGER, 'undefined').
-define(DEF_POOL_ERROR_LOGGER, 'undefined').
-define(DEF_CONNECTION_COUNT, 1).
-define(DEF_CONNECTION_PER_ADDRESS, 1).
-define(DEF_TABLE_TYPE, 'list').
-define(DEF_ADDRESSES, []).

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), etcp_types:init_argument(), etcp_types:addresses()) ->
    etcp_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Mod, InitArg, Addrs) when erlang:is_atom(Mod),
                                     erlang:is_list(Addrs) ->
    director:start_link(?MODULE, {Mod, InitArg, Addrs, ?DEF_START_OPTIONS}).


-spec
start_link(etcp_types:register_name() | module()
          ,module() | etcp_types:init_argument()
          , etcp_types:init_argument() | etcp_types:addresses()
          , etcp_types:addresses() | etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Name, Mod, InitArg, Addrs) when erlang:is_tuple(Name),
                                           erlang:is_atom(Mod),
                                           erlang:is_map(Addrs) ->
    director:start_link(Name, ?MODULE, {Mod, InitArg, Addrs, ?DEF_START_OPTIONS});
start_link(Mod, InitArg, Addrs, Opts) when erlang:is_atom(Mod),
                                           erlang:is_list(Addrs),
                                           erlang:is_map(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Addrs, Opts}).


-spec
start_link(etcp_types:register_name()
          ,module()
          , etcp_types:init_argument()
          , etcp_types:addresses()
          , etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Name, Mod, InitArg, Addrs, Opts) when erlang:is_tuple(Name),
                                                 erlang:is_atom(Mod),
                                                 erlang:is_list(Addrs),
                                                 erlang:is_map(Opts) ->
    director:start_link(Name, ?MODULE, {Mod, InitArg, Addrs, Opts}).


-spec
fetch(etcp_types:name(), etcp_types:connection_table_type()) ->
    [] | [{reference(), pid()}].
%% @doc
%%      fetch all available connections with their pids.
%% @end
fetch(ConSup, list) when ?is_proc_ref(ConSup) ->
    {ok, Pids} = director:get_pids(ConSup),
    Pids;
fetch(ConSup, {ets, Tab}) when ?is_proc_ref(ConSup) ->
    {ok, Pids} = director_table_ets:get_pids(Tab),
    Pids;
fetch(ConSup, {mnesia, Tab}) when ?is_proc_ref(ConSup) ->
    {ok, Pids} = director_table_ets:get_pids(Tab),
    Pids.


-spec
add(etcp_types:name(), etcp_types:host(), etcp_types:port_number()) ->
    etcp_types:start_return().
%% @doc
%%      Adds new connection for Host:Port in pool.
%% @end
add(ConSup, Host, Port) when ?is_proc_ref(ConSup) andalso
                             ?is_host(Host) andalso
                             erlang:is_integer(Port) ->
    director:start_child(ConSup, #{id => erlang:make_ref()
                                  ,start => {etcp_connection, start_link, [Host, Port]}
                                  ,append => true}).


-spec
add(etcp_types:name(), etcp_types:host(), etcp_types:port_number(), pos_integer()) ->
    [etcp_types:start_return()].
%% @doc
%%      Adds new connections for Host:Port in pool.
%% @end
add(ConSup, Host, Port, Count) when ?is_proc_ref(ConSup) andalso
                                    ?is_host(Host) andalso
                                    erlang:is_integer(Port) andalso
                                    (erlang:is_integer(Count) andalso Count > 0) ->
    Fun =
        fun(_, Acc) ->
            [add(ConSup, Host, Port)|Acc]
        end,
    lists:foldl(Fun, [], lists:seq(1, Count)).


-spec
stop(etcp_types:name()) ->
    'ok'.
%% @doc
%%      stops pool and all of its connections.
%% @end
stop(ConSup) when ?is_proc_ref(ConSup) ->
    director:stop(ConSup, normal, ?DEF_TERMINATE_TIMEOUT).


-spec
stop(etcp_types:name(), Reason:: etcp_types:reason()) ->
    ok.
%% @doc
%%      stops pool and all of its connections.
%% @end
stop(ConSup, Reason) when ?is_proc_ref(ConSup) ->
    director:stop(ConSup, Reason, ?DEF_TERMINATE_TIMEOUT).

%% -------------------------------------------------------------------------------------------------
%% 'etcp_server_connection_sup' callbacks:

%% @hidden
start_link_(Mod, InitArg, Opts) when erlang:is_atom(Mod) andalso
                                     erlang:is_map(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts}, ?DEF_START_OPTIONS).

%% -------------------------------------------------------------------------------------------------
%% 'etcp_acceptor' callbacks:

%% @hidden
add_(ConSup, Sock) ->
    director:start_child(ConSup, #{id => erlang:make_ref()
                                  ,start => {etcp_connection, start_link, [Sock]}
                                  ,append => true}).

%% -------------------------------------------------------------------------------------------------
%% 'director' callback:

%% @hidden
init({Mod, InitArg, Opts}) ->
    ConPlan = etcp_utils:get_value(connection_childspec_plan
                                  ,Opts
                                  ,?DEF_CHILDSPEC_PLAN
                                  ,fun etcp_utils:filter_plan/1),
    ConErrorLogger = etcp_utils:get_value(connection_error_logger
                                         ,Opts
                                         ,?DEF_CHILDSPEC_ERROR_LOGGER
                                         ,fun etcp_utils:filter_logger2/1),
    TabType = etcp_utils:get_value(connection_table_type
                                  ,Opts
                                  ,?DEF_TABLE_TYPE
                                  ,fun etcp_utils:filter_table_type/1),
    DefChildSpec = #{start => {etcp_connection, start_link, [Mod, InitArg, Opts]}
                    ,state => {ConPlan, ConErrorLogger}
                    ,plan => fun plan_runner/4
                    ,type => worker
                    ,log_validator => fun child_log_validator/4},
    Opts2 = [{log_validator, fun sup_log_validator/4}
            ,{delete_table_before_terminate, false}] ++ TabType,
    {ok, undefined, [], DefChildSpec, Opts2};
init({Mod, InitArg, Addrs0, Opts}) ->

    Addrs = etcp_utils:get_value(addresses
                                ,#{addresses => Addrs0}
                                ,undefined
                                ,fun filter_addresses/1),

    ConCount = etcp_utils:get_value(connection_per_address
                                   ,Opts
                                   ,?DEF_CONNECTION_PER_ADDRESS
                                   ,fun etcp_utils:is_whole_integer/1),
    ConPlan = etcp_utils:get_value(connection_childspec_plan
                                  ,Opts
                                  ,?DEF_CHILDSPEC_PLAN
                                  ,fun etcp_utils:filter_plan/1),
    ConErrorLogger = etcp_utils:get_value(connection_error_logger
                                         ,Opts
                                         ,?DEF_CHILDSPEC_ERROR_LOGGER
                                         ,fun etcp_utils:filter_logger2/1),
    TabType = etcp_utils:get_value(connection_table_type
                                  ,Opts
                                  ,?DEF_TABLE_TYPE
                                  ,fun etcp_utils:filter_table_type/1),
    PoolErrorLogger = etcp_utils:get_value(pool_error_logger
                                          ,Opts
                                          ,?DEF_POOL_ERROR_LOGGER
                                          ,fun etcp_utils:filter_logger/1),
    ChildSpecs = [[#{id => erlang:make_ref()
                    ,start => {etcp_connection
                              ,start_link
                              ,[Mod, InitArg, Host, Port, Opts]}
                    ,state => {ConPlan, ConErrorLogger}
                    ,plan => fun plan_runner/4
                    ,type => worker} || _ <- lists:seq(1, ConCount)]
                  || {Host, Port} <- Addrs],
    DefChildSpec = #{start => {etcp_connection, start_link, [Mod, InitArg, Opts]}
                    ,state => {ConPlan, ConErrorLogger}
                    ,plan => fun pool_plan_runner/4
                    ,type => worker
                    ,log_validator => fun pool_child_log_validator/4},
    Opts2 = [{log_validator, fun pool_sup_log_validator/4}
            ,{delete_table_before_terminate, false}] ++ TabType,
    {ok, PoolErrorLogger, lists:concat(ChildSpecs), DefChildSpec, Opts2}.


terminate(_Rsn, _St) ->
    ok.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

filter_addresses(Addrs) when erlang:is_list(Addrs) ->
    filter_addresses(Addrs, []);
filter_addresses(_) ->
    false.


filter_addresses([{_Host, _Port}=Addr|Addrs], Addrs2) ->
    filter_addresses(Addrs, [Addr|Addrs2]);
filter_addresses([], Addrs2) ->
    {ok, lists:reverse(Addrs2)};
filter_addresses([Other|_Addrs], _Addrs) ->
    {error, {address_format, [{address, Other}]}}.


plan_runner(_Id, normal, _RestartCount, {undefined, _}=St) ->
    {delete, St};
plan_runner(_Id, _Rsn, _RestartCount, {undefined, _}=St) ->
    {stop, St};
plan_runner(_Id, Rsn, _RestartCount, {Plan, _}=St) ->
    try Plan(Rsn) of
        delete ->
            {delete, St};
        stop ->
            {stop, St};
        {stop, _}=Stop ->
            {Stop, St};
        Other ->
            error_logger:format("ETCP connection: plan returns unknown value ~p\n", [Other]),
            {delete, St}
    catch
        _:Rsn2 ->
            error_logger:format("ETCP connection: plan crashed with reason ~p and stacktrace ~p with"
                                " ~p as reason argument\n"
                               ,[Rsn2, erlang:get_stacktrace(), Rsn]),
            {delete, St}
    end.


child_log_validator(_Id, info, start, _State) ->
    short;
child_log_validator(_Id, error, normal, {_, undefined}) ->
    short;
child_log_validator(_Id, error, _Rsn, {_, undefined}) ->
    long;
child_log_validator(_Id, error, Rsn, {_, Logger}) ->
    try
        Logger(Rsn),
        none
    catch
        _:Rsn2 ->
            error_logger:format("ETCP connection: Error logger crashed with reason ~p and stacktrace"
                                " ~p\n"
                               ,[Rsn2, erlang:get_stacktrace()]),
            long
    end.


sup_log_validator(_Name, error, _, _State) ->
    none;
sup_log_validator(_Name, warning, _, _State) ->
    long.


pool_plan_runner(_Id, normal, _RestartCount, {undefined, _}=St) ->
    {delete, St};
pool_plan_runner(_Id, _Rsn, _RestartCount, {undefined, _}=St) ->
    {stop, St};
pool_plan_runner(_Id, Rsn, _RestartCount, {Plan, _}=St) ->
    try Plan(Rsn) of
        delete ->
            {delete, St};
        stop ->
            {stop, St};
        {stop, _}=Stop ->
            {Stop, St};
        {restart, Int}=Restart when erlang:is_integer(Int) andalso Int > -1 ->
            {Restart, St};
        restart ->
            {restart, St};
        Other ->
            error_logger:format("ETCP connection: plan returns unknown value ~p\n", [Other]),
            {delete, St}
    catch
        _:Rsn2 ->
            error_logger:format("ETCP connection: plan crashed with reason ~p and stacktrace ~p with"
                                " ~p as reason argument\n"
                               ,[Rsn2, erlang:get_stacktrace(), Rsn]),
            {delete, St}
    end.


pool_child_log_validator(_Id, info, start, _State) ->
    short;
pool_child_log_validator(_Id, error, normal, {_, undefined}) ->
    short;
pool_child_log_validator(_Id, error, _Rsn, {_, undefined}) ->
    long;
pool_child_log_validator(_Id, error, Rsn, {_, Logger}) ->
    try
        Logger(Rsn),
        none
    catch
        _:Rsn2 ->
            error_logger:format("ETCP connection ~p: Error logger crashed with reason ~p and stacktr"
                                "ace ~p\n"
                               ,[Rsn2, erlang:get_stacktrace()]),
            long
    end.


pool_sup_log_validator(_Name, error, normal, undefined) ->
    short;
pool_sup_log_validator(_Name, error, _Rsn, undefined) ->
    long;
pool_sup_log_validator(Name, error, Rsn, Logger) ->
    try
        Logger(Name, Rsn),
        none
    catch
        _:Rsn2 ->
            error_logger:format("ETCP connection pool ~p: Error logger crashed with reason ~p and s"
                                "tacktrace ~p\n"
                               ,[Name, Rsn2, erlang:get_stacktrace()]),
            long
    end;
pool_sup_log_validator(_Name, warning, _, _State) ->
    long.
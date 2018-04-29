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
-module(etcp_connection_pool).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/3
        ,start_link/4
        ,start_link/5
        ,add/3
        ,fetch/1
        ,stop/1
        ,stop/2]).

%% internal callback:
-export([start_link_/5]).

%% 'director' callback:
-export([init/1
        ,handle_start/4
        ,handle_exit/5
        ,handle_terminate/5
        ,terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {module}).

-define(DEF_START_OPTIONS, #{}).
-define(DEF_TERMINATE_TIMEOUT, 'infinity').
-define(DEF_CONNECTION_COUNT, 1).

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), etcp_types:init_argument(), etcp_types:addresses()) ->
    etcp_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Mod, InitArg, Addrs) when erlang:is_atom(Mod) andalso erlang:is_list(Addrs) ->
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
fetch(etcp_types:name()) ->
    [] | [{{reference(), etcp_types:address()}, pid()}].
%% @doc
%%      fetch all available connections with their pids.
%% @end
fetch(ConSup) when ?is_proc_ref(ConSup) ->
    {ok, Pids} = director:get_pids(ConSup),
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
    director:start_child(ConSup, #{id => {erlang:make_ref(), {Host, Port}}
                                  ,start => {?MODULE, start_link_, [Host, Port]}
                                  ,append => true}).


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
start_link_(Mod, InitArg, Opts, Host, Port) ->
    etcp_connection:start_link(Mod, InitArg, Host, Port, Opts).

%% -------------------------------------------------------------------------------------------------
%% 'director' callback:

%% @hidden
init({Mod, InitArg, Addrs0, Opts}) ->
    Addrs = etcp_utils:get_value(addresses
                                ,#{addresses => Addrs0}
                                ,undefined
                                ,fun filter_addresses/1),
    ChildSpecs = [#{id => {erlang:make_ref(), Addr}
                   ,start => {?MODULE, start_link_, [Mod, InitArg, Opts, Host, Port]}
                   ,state => InitArg}
                 || {Host, Port}=Addr <- Addrs],
    DefChildSpec = #{start => {?MODULE, start_link_, [Mod, InitArg, Opts]}},
    {ok, #?S{module = Mod}, ChildSpecs, DefChildSpec}.


%% @hidden
handle_start(_, ChildState, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
handle_exit(Id, ChildState, Rsn, #?S{module = Mod}=State, _) ->
    {ChildState2, Action} =
        try Mod:handle_exit(Rsn, Id, ChildState) of
            {_, restart}=Ok ->
                Ok;
            {_, {restart, Int}}=Ok when erlang:is_integer(Int) andalso Int > -1 ->
                Ok;
            {_, delete}=Ok ->
                Ok;
            {_, {stop, _}}=Ok ->
                Ok;
            Other ->
                {ChildState, {stop, {return, [{value, Other}
                                             ,{module, Mod}
                                             ,{function, handle_exit}
                                             ,{arguments, [Rsn, Id, ChildState]}]}}}
        catch
            _:Rsn ->
                {ChildState, {stop, {crash, [{reason, Rsn}
                                            ,{stacktrace, erlang:get_stacktrace()}
                                            ,{module, Mod}
                                            ,{function, handle_exit}
                                            ,{arguments, [Rsn, Id, ChildState]}]}}}
        end,
    {Action, ChildState2, State, [{log, false}]}.


%% @hidden
handle_terminate(_, ChildState, _, State, _) ->
    {ok, ChildState, State, [{log, false}]}.


%% @hidden
terminate(Rsn, _) ->
    LogFlag =
        if
            Rsn =:= normal orelse Rsn =:= shutdown ->
                false;
            true ->
                true
        end,
    {ok, [{log, LogFlag}]}.

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
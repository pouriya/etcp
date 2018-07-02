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
%%          Advanced Erlang socket library for TCP protocols which provides fast, efficient and<br/>
%%          simple API for implementing servers, clients and connection pools.<br/>
%%          For implementing server, connection pool or stand-alone connection, etcp behavior<br/>
%%          should be implemented.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(etcp).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% Server API
-export([start_link_server/3
        ,start_link_server/4
        ,start_link_server/5
        ,stop_server/1
        ,stop_server/2
        ,fetch_acceptors/1
        ,suspend/1
        ,resume/1
        ,modes/1
        ,fetch_server_connections/1]).

%% Client connection pool API:
-export([start_link_connection_pool/3
        ,start_link_connection_pool/4
        ,start_link_connection_pool/5
        ,stop_connection_pool/1
        ,stop_connection_pool/2
        ,fetch_pool_connections/1
        ,add_connection/3]).

%% Client API:
-export([start_link_connection/4
        ,start_link_connection/5
        ,start_link_connection/6
        ,stop_connection/1
        ,stop_connection/2
        ,stop_connection/3
        ,send_sync/2
        ,send_sync/3
        ,send_async/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% Types:

-opaque name() :: pid() | atom() | tuple().

-type init_argument() :: any().

-opaque start_return() :: {'ok', pid()} | {'error', term()} | 'ignore'.

-opaque register_name() :: {'local', atom()}
                         | {'global', atom()}
                         | {'via', module(), term()}.

-opaque start_options() :: #{'acceptor_count' => pos_integer()
                            ,'acceptor_mode' => acceptor_mode()
                            ,'acceptor_debug' => debug()
                            ,'transporter' => atom() % tcp, ssl, your own
                            ,'transporter_options' => transporter_options()
                            ,'connection_debug' => debug()
                            ,'connection_process_keeper' => boolean()}.

-opaque acceptor_mode() :: 'accept' | 'sleep'.

-opaque acceptor_id() :: pos_integer().

-opaque process_registry() :: pid() | {'apply', {module(), atom()}}.

-opaque debug() :: [sys:dbg_opt()] | [].

-type transporter_options() :: term(). % Depends on transporter
                                       % for tcp use gen_tcp:options()
                                       % for ssl use ssl:options()

-opaque addresses() :: [] | [address()].
-opaque  address() :: {hostname(), port_number()} | {'undefined', 'undefined'}.
-opaque   port_number() :: inet:port_number().
-opaque   hostname() :: inet:hostname() | inet:socket_address().

-type packet() :: any(). % Depends on transporter
                         % for tcp and ssl it must be:
                         % string() | binary | iolist()

-export_type([name/0
             ,start_return/0
             ,register_name/0
             ,start_options/0
             ,port_number/0
             ,hostname/0
             ,acceptor_mode/0
             ,acceptor_id/0
             ,process_registry/0
             ,addresses/0
             ,address/0
             ,init_argument/0
             ,debug/0
             ,transporter_options/0
             ,packet/0]).

%% -------------------------------------------------------------------------------------------------
%% API functions:

-spec
start_link_server(module(), init_argument(), port_number()) ->
    start_return().
%% @doc
%%      Starts and links a TCP server.
%% @end
start_link_server(Mod, InitArg, Port) when erlang:is_atom(Mod),
                                           erlang:is_integer(Port) ->
    etcp_server:start_link(Mod, InitArg, Port).


-spec
start_link_server(register_name() | module()
                 ,module()        | init_argument()
                 ,init_argument() | port_number()
                 ,port_number()   | start_options()) ->
    start_return().
%% @doc
%%      Starts and links a TCP server.
%% @end
start_link_server(Name, Mod, InitArg, Port) when erlang:is_tuple(Name) andalso
                                                 erlang:is_atom(Mod)   andalso
                                                 erlang:is_integer(Port)    ->
    etcp_server:start_link(Name, Mod, InitArg, Port);

start_link_server(Mod, InitArg, Port, Opts) when erlang:is_atom(Mod)     andalso
                                                 erlang:is_integer(Port) andalso
                                                 erlang:is_map(Opts)          ->
    etcp_server:start_link(Mod, InitArg, Port, Opts).



-spec
start_link_server(register_name()
                 ,module()
                 ,init_argument()
                 ,port_number()
                 ,start_options()) ->
    start_return().
%% @doc
%%      Starts and links a TCP server.
%% @end
start_link_server(Name, Mod, InitArg, Port, Opts) when erlang:is_tuple(Name)   andalso
                                                       erlang:is_atom(Mod)     andalso
                                                       erlang:is_integer(Port) andalso
                                                       erlang:is_map(Opts)          ->
    etcp_server:start_link(Name, Mod, InitArg, Port, Opts).


-spec
fetch_server_connections(name()) ->
    [] | [pid()].
%% @doc
%%      Returns all available server connections.
%% @end
fetch_server_connections(Server) when ?is_proc_ref(Server) ->
    etcp_server:connections(Server).


-spec
fetch_acceptors(name()) ->
    [] | [{acceptor_id(), pid()}].
%% @doc
%%      Returns all server acceptors.
%% @end
fetch_acceptors(Server) when ?is_proc_ref(Server) ->
    etcp_server:acceptors(Server).


-spec
suspend(name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to sleep mode.
%% @end
suspend(Server) when ?is_proc_ref(Server) ->
    etcp_server:suspend(Server).


-spec
resume(name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to accept mode.
%% @end
resume(Server) when ?is_proc_ref(Server) ->
    etcp_server:resume(Server).


-spec
modes(name()) ->
    acceptor_mode() | [{acceptor_id(), acceptor_mode()}].
%% @doc
%%      Returns mode(s) of server acceptors.
%% @end
modes(Server) when ?is_proc_ref(Server) ->
    etcp_server:modes(Server).


-spec
stop_server(name()) ->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server) when ?is_proc_ref(Server) ->
    etcp_server:stop(Server).


-spec
stop_server(name(), term())->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server, Reason) when ?is_proc_ref(Server) ->
    etcp_server:stop(Server, Reason).


-spec
start_link_connection_pool(module(), init_argument(), addresses()) ->
    start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connection_pool(Mod, InitArg, Addrs) when erlang:is_atom(Mod) andalso
                                                     erlang:is_list(Addrs) ->
    etcp_connection_pool:start_link(Mod, InitArg, Addrs).


-spec
start_link_connection_pool(register_name() | module()
                          ,module()        | init_argument()
                          ,init_argument() | addresses()
                          ,addresses()     | start_options()) ->
    start_return().
%% @doc
%%      Starts and links a TCP connection pool.
%% @end
start_link_connection_pool(Name, Mod, InitArg, Addrs) when erlang:is_tuple(Name) andalso
                                                           erlang:is_atom(Mod)   andalso
                                                           erlang:is_list(Addrs)      ->
    etcp_connection_pool:start_link(Name, Mod, InitArg, Addrs);

start_link_connection_pool(Mod, InitArg, Addrs, Opts) when erlang:is_atom(Mod)   andalso
                                                           erlang:is_list(Addrs) andalso
                                                           erlang:is_map(Opts)        ->
    etcp_connection_pool:start_link(Mod, InitArg, Addrs, Opts).

-spec
start_link_connection_pool(register_name()
                          ,module()
                          ,init_argument()
                          ,addresses()
                          ,start_options()) ->
    start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connection_pool(Name, Mod, InitArg, Addrs, Opts) when erlang:is_tuple(Name) andalso
                                                                 erlang:is_atom(Mod)   andalso
                                                                 erlang:is_list(Addrs) andalso
                                                                 erlang:is_map(Opts)        ->
    etcp_connection_pool:start_link(Name, Mod, InitArg, Addrs, Opts).


-spec
fetch_pool_connections(name()) ->
    [] | [{{reference(), address()}, pid()}].
%% @doc
%%      Returns all available pool connections.
%% @end
fetch_pool_connections(Pool) when ?is_proc_ref(Pool) ->
    etcp_connection_pool:fetch(Pool).


-spec
add_connection(name(), hostname(), port_number()) ->
    start_return().
%% @doc
%%      Adds new connection for Host:Port in pool.
%% @end
add_connection(Pool, Host, Port) when ?is_proc_ref(Pool) andalso
                                      ?is_host(Host)     andalso
                                      erlang:is_integer(Port) ->
    etcp_connection_pool:add(Pool, Host, Port).


-spec
stop_connection_pool(name()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has.
%% @end
stop_connection_pool(Pool) when ?is_proc_ref(Pool) ->
    etcp_connection_pool:stop(Pool).


-spec
stop_connection_pool(name(), term()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has with specific reason.
%% @end
stop_connection_pool(Pool, Reason) when ?is_proc_ref(Pool) ->
    etcp_connection_pool:stop(Pool, Reason).


-spec
start_link_connection(module()
                     ,init_argument()
                     ,hostname()
                     ,port_number()) ->
    start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Mod, InitArg, Host, Port)  when erlang:is_atom(Mod) andalso
                                                      ?is_host(Host)      andalso
                                                      erlang:is_integer(Port)  ->
    etcp_connection:start_link(Mod, InitArg, Host, Port).


-spec
start_link_connection(register_name() | module()
                     ,module()        | init_argument()
                     ,init_argument() | hostname()
                     ,hostname()      | port_number()
                     ,port_number()   | start_options()) ->
    start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Name, Mod, InitArg, Host, Port) when erlang:is_tuple(Name) andalso
                                                           erlang:is_atom(Mod)   andalso
                                                           ?is_host(Host)        andalso
                                                           erlang:is_integer(Port)    ->
    etcp_connection:start_link(Name, Mod, InitArg, Host, Port);

start_link_connection(Mod, InitArg, Host, Port, Opts) when erlang:is_atom(Mod)     andalso
                                                           ?is_host(Host)          andalso
                                                           erlang:is_integer(Port) andalso
                                                           erlang:is_map(Opts)          ->
    etcp_connection:start_link(Mod, InitArg, Host, Port, Opts).


-spec
start_link_connection(register_name()
                     ,module()
                     ,init_argument()
                     ,hostname()
                     ,port_number()
                     ,start_options()) ->
    start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Name, Mod, InitArg, Host, Port, Opts) when erlang:is_tuple(Name)   andalso
                                                                 erlang:is_atom(Mod)     andalso
                                                                 ?is_host(Host)          andalso
                                                                 erlang:is_integer(Port) andalso
                                                                 erlang:is_map(Opts)          ->
    etcp_connection:start_link(Name, Mod, InitArg, Host, Port, Opts).


-spec
send_sync(name(), packet()) ->
    'ok' | {error, {atom(), [] | [{atom(), term()}]} | term()}.
%% @doc
%%      Sends packet synchronously through connection.
%% @end
send_sync(Con, Packet) when ?is_proc_ref(Con) ->
    etcp_connection:send_sync(Con, Packet).


-spec
send_sync(name(), packet(), timeout()) ->
    'ok' | {error, {atom(), [] | [{atom(), term()}]} | term()}.
%% @doc
%%      Sends packet synchronously through connection with timeout.
%% @end
send_sync(Con, Packet, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
    etcp_connection:send_sync(Con, Packet, Timeout).


-spec
send_async(name(), packet()) ->
    'ok'.
%% @doc
%%      Sends packet asynchronously through connection.
%% @end
send_async(Con, Packet) when ?is_proc_ref(Con) ->
    etcp_connection:send_async(Con, Packet).


-spec
stop_connection(name()) ->
    'ok'.
%% @doc
%%      Stops connection.
%% @end
stop_connection(Con) when ?is_proc_ref(Con) ->
    etcp_connection:stop(Con).


-spec
stop_connection(name(), term()) ->
    'ok'.
%% @doc
%%      Stops connection with specific reason.
%% @end
stop_connection(Con, Reason) when ?is_proc_ref(Con) ->
    etcp_connection:stop(Con, Reason).


-spec
stop_connection(name(), term(), timeout()) ->
    'ok'.
%% @doc
%%      Stops connection with specific reason.
%% @end
stop_connection(Con, Reason, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
    etcp_connection:stop(Con, Reason, Timeout).
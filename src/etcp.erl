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
        ,sleep/1
        ,accept/1
        ,modes/1
        ,fetch_server_connections/1]).

%% Client connection pool API:
-export([start_link_connection_pool/3
        ,start_link_connection_pool/4
        ,start_link_connection_pool/5
        ,stop_connection_pool/1
        ,stop_connection_pool/2
        ,fetch_pool_connections/2
        ,add_connection/3
        ,add_connections/4]).

%% Client API:
-export([start_link_connection/4
        ,start_link_connection/5
        ,start_link_connection/6
        ,send_sync/2
        ,send_sync/3
        ,send_async/2
        ,stop_connection/1
        ,stop_connection/2
        ,stop_connection/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% Behaviour info:

%% Mandatory for server implementation.
%%-callback
%%listen_init(etcp_types:init_argument()
%%           ,etcp_types:port_number()
%%           ,etcp_types:start_options()
%%           ,etcp_types:socket()) ->
%%    etcp_types:listen_init_callback_return().


-callback
connection_init(etcp_types:init_argument(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% When a TCP packet arrives.
-callback
handle_packet(etcp_types:packet(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% When you want to send Erlang call to connection handler processes.
-callback
handle_call(Request::any(), From::tuple(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% When you want to send cast to connection handler processes.
-callback
handle_cast(Cast::any(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% When you want to send event to connection handler processes. Message in form of {'$gen_event', _}
-callback
handle_event(Event::any(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% When you want to send other Erlang messages to connection handler processes.
-callback
handle_info(Msg::any(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


%% Useful you want to use 'timeout' option in return value of other callbacks.
%%-callback
%%timeout(etcp_types:state(), etcp_types:metadata()) ->
%%    etcp_types:callback_return().


%% Useful you want to use 'srtimeout' option in return value of other callbacks when you are using
%% passive sockets.
%%-callback
%%srtimeout(etcp_types:state(), etcp_types:metadata()) ->
%%    etcp_types:callback_return().


%% Will be called when connection is closed by other side.
-callback
handle_disconnect(etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:callback_return().


-callback
terminate(etcp_types:reason(), etcp_types:state(), etcp_types:metadata()) ->
    etcp_types:terminate_callback_return().


-callback
code_change(OldVsn::any(), etcp_types:state(), Extra::any()) ->
    etcp_types:code_change_callback_return().

%% -------------------------------------------------------------------------------------------------
%% API functions:

-spec
start_link_server(module(), etcp_types:init_argument(), etcp_types:port_number()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Mod, InitArg, Port) when erlang:is_atom(Mod),
                                           erlang:is_integer(Port) ->
    etcp_server_sup:start_link(Mod, InitArg, Port).


-spec
start_link_server(etcp_types:register_name() | module()
                 ,module() | etcp_types:init_argument()
                 , etcp_types:init_argument() | etcp_types:port_number()
                 , etcp_types:port_number() | etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Name_or_Mod
                 ,Mod_or_InitArg
                 ,InitArg_or_Port
                 ,Port_or_Opts) when (erlang:is_tuple(Name_or_Mod) andalso
                                      erlang:is_atom(Mod_or_InitArg) andalso
                                      erlang:is_integer(Port_or_Opts)) orelse
                                     (erlang:is_atom(Name_or_Mod) andalso
                                      erlang:is_integer(InitArg_or_Port) andalso
                                      erlang:is_map(Port_or_Opts))->
    etcp_server_sup:start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Port, Port_or_Opts).


-spec
start_link_server(etcp_types:register_name()
                 ,module()
                 , etcp_types:init_argument()
                 , etcp_types:port_number()
                 , etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Name, Mod, InitArg, Port, Opts) when erlang:is_tuple(Name) andalso
                                                       erlang:is_atom(Mod) andalso
                                                       erlang:is_integer(Port) andalso
                                                       erlang:is_map(Opts) ->
    etcp_server_sup:start_link(Name, Mod, InitArg, Port, Opts).


-spec
fetch_server_connections(etcp_types:name()) ->
    [] | [{reference(), pid()}].
%% @doc
%%      Returns all available server connections.
%% @end
fetch_server_connections(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:connections(Server).


-spec
fetch_acceptors(etcp_types:name()) ->
    [] | [{reference(), pid()}].
%% @doc
%%      Returns all server acceptors.
%% @end
fetch_acceptors(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:acceptors(Server).


-spec
sleep(etcp_types:name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to sleep mode.
%% @end
sleep(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:sleep(Server).


-spec
accept(etcp_types:name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to accept mode.
%% @end
accept(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:accept(Server).


-spec
modes(etcp_types:name()) ->
    etcp_types:acceptor_mode() | [etcp_types:acceptor_mode()].
%% @doc
%%      Returns mode(s) of server acceptors.
%% @end
modes(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:modes(Server).


-spec
stop_server(etcp_types:name()) ->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server) when ?is_proc_ref(Server) ->
    etcp_server_sup:stop(Server).


-spec
stop_server(etcp_types:name(), etcp_types:reason())->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server, Reason) when ?is_proc_ref(Server) ->
    etcp_server_sup:stop(Server, Reason).


-spec
start_link_connection_pool(module(), etcp_types:init_argument(), etcp_types:addresses()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connection_pool(Mod, InitArg, Addrs) when erlang:is_atom(Mod) andalso
                                                    erlang:is_list(Addrs) ->
    etcp_connection_sup:start_link(Mod, InitArg, Addrs).


-spec
start_link_connection_pool(etcp_types:register_name()| module()
                         ,module() | etcp_types:init_argument()
                         ,etcp_types:init_argument() | etcp_types:addresses()
                         ,etcp_types:addresses() | etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connection_pool(Name_or_Mod
                         ,Mod_or_InitArg
                         ,InitArg_or_Addrs
                         ,Addrs_or_Opts) when (erlang:is_tuple(Name_or_Mod) andalso
                                               erlang:is_atom(Mod_or_InitArg) andalso
                                               erlang:is_list(Addrs_or_Opts)) orelse
                                              (erlang:is_atom(Name_or_Mod) andalso
                                               erlang:is_list(InitArg_or_Addrs) andalso
                                               erlang:is_map(Addrs_or_Opts)) ->
    etcp_connection_sup:start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Addrs, Addrs_or_Opts).


-spec
start_link_connection_pool(etcp_types:register_name()
                         ,module()
                         ,etcp_types:init_argument()
                         ,etcp_types:addresses()
                         ,etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connection_pool(Name, Mod, InitArg, Addrs, Opts) when erlang:is_tuple(Name) andalso
                                                                 erlang:is_atom(Mod) andalso
                                                                 erlang:is_list(Addrs) andalso
                                                                 erlang:is_map(Opts) ->
    etcp_connection_sup:start_link(Name, Mod, InitArg, Addrs, Opts).


-spec
fetch_pool_connections(etcp_types:name(), etcp_types:connection_table_type()) ->
    [] | [{reference(), pid()}].
%% @doc
%%      Returns all available pool connections.
%% @end
fetch_pool_connections(Pool, ConTabType) when ?is_proc_ref(Pool) andalso
                                              ?is_table_type(ConTabType) ->
    etcp_connection_sup:fetch(Pool, ConTabType).


-spec
add_connection(etcp_types:name(), etcp_types:host(), etcp_types:port_number()) ->
    etcp_types:start_return().
%% @doc
%%      Adds new connection for Host:Port in pool.
%% @end
add_connection(Pool, Host, Port) when ?is_proc_ref(Pool) andalso
                                     ?is_host(Host) andalso
                                     erlang:is_integer(Port) ->
    etcp_connection_sup:add(Pool, Host, Port).


-spec
add_connections(etcp_types:name()
               ,etcp_types:host()
               ,etcp_types:port_number()
               ,pos_integer()) ->
    [etcp_types:start_return()].
%% @doc
%%      Adds new connections for Host:Port in pool.
%% @end
add_connections(Pool, Host, Port, Count) when ?is_proc_ref(Pool) andalso
                                              ?is_host(Host) andalso
                                              erlang:is_integer(Port) andalso
                                              (erlang:is_integer(Count) andalso Count > 0) ->
    etcp_connection_sup:add(Pool, Host, Port, Count).


-spec
stop_connection_pool(etcp_types:name()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has.
%% @end
stop_connection_pool(Pool) when ?is_proc_ref(Pool) ->
    etcp_connection_sup:stop(Pool).


-spec
stop_connection_pool(etcp_types:name(), etcp_types:reason()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has with specific reason.
%% @end
stop_connection_pool(Pool, Reason) when ?is_proc_ref(Pool) ->
    etcp_connection_sup:stop(Pool, Reason).


-spec
start_link_connection(module()
                     ,etcp_types:init_argument()
                     ,etcp_types:hostname()
                     ,etcp_types:port_number()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Mod, InitArg, Host, Port)  when erlang:is_atom(Mod) andalso
                                                     ?is_host(Host) andalso
                                                     erlang:is_integer(Port) ->
    etcp_connection:start_link(Mod, InitArg, Host, Port).


-spec
start_link_connection(etcp_types:register_name() | module()
                     ,module() | etcp_types:init_argument()
                     ,etcp_types:init_argument() | etcp_types:hostname()
                     ,etcp_types:hostname() | etcp_types:port_number()
                     ,etcp_types:port_number() | etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Name_or_Mod
                     ,Mod_or_InitArg
                     ,InitArg_or_Host
                     ,Host_or_Port
                     ,Port_or_Opts) when (erlang:is_tuple(Name_or_Mod) andalso
                                          erlang:is_atom(Mod_or_InitArg) andalso
                                          ?is_host(Host_or_Port) andalso
                                          erlang:is_integer(Port_or_Opts)) orelse
                                          (erlang:is_atom(Name_or_Mod) andalso
                                          ?is_host(InitArg_or_Host) andalso
                                          erlang:is_integer(Host_or_Port) andalso
                                          erlang:is_map(Port_or_Opts)) ->
    etcp_connection:start_link(Name_or_Mod
                              ,Mod_or_InitArg
                              ,InitArg_or_Host
                              ,Host_or_Port
                              ,Port_or_Opts).


-spec
start_link_connection(etcp_types:register_name()
                     ,module()
                     ,etcp_types:init_argument()
                     ,etcp_types:hostname()
                     ,etcp_types:port_number()
                     ,etcp_types:start_options()) ->
    etcp_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connection(Name, Mod, InitArg, Host, Port, Opts) when erlang:is_tuple(Name) andalso
                                                                 erlang:is_atom(Mod) andalso
                                                                 ?is_host(Host) andalso
                                                                 erlang:is_integer(Port) andalso
                                                                 erlang:is_map(Opts) ->
    etcp_connection:start_link(Name, Mod, InitArg, Host, Port, Opts).


-spec
send_sync(etcp_types:name(), etcp_types:packet()) ->
    'ok' | etcp_types:error().
%% @doc
%%      Sends packet synchronously through connection.
%% @end
send_sync(Con, Packet) when ?is_proc_ref(Con) ->
    etcp_connection:send_sync(Con, Packet).


-spec
send_sync(etcp_types:name(), etcp_types:packet(), timeout()) ->
    'ok' | etcp_types:error().
%% @doc
%%      Sends packet synchronously through connection with timeout.
%% @end
send_sync(Con, Packet, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
    etcp_connection:send_sync(Con, Packet, Timeout).


-spec
send_async(etcp_types:name(), etcp_types:packet()) ->
    'ok'.
%% @doc
%%      Sends packet asynchronously through connection.
%% @end
send_async(Con, Packet) when ?is_proc_ref(Con) ->
    etcp_connection:send_async(Con, Packet).


-spec
stop_connection(etcp_types:name()) ->
    'ok'.
%% @doc
%%      Stops connection.
%% @end
stop_connection(Con) when ?is_proc_ref(Con) ->
    etcp_connection:stop(Con).


-spec
stop_connection(etcp_types:name(), etcp_types:reason()) ->
    'ok'.
%% @doc
%%      Stops connection with specific reason.
%% @end
stop_connection(Con, Reason) when ?is_proc_ref(Con) ->
    etcp_connection:stop(Con, Reason).


-spec
stop_connection(etcp_types:name(), etcp_types:reason(), timeout()) ->
    'ok'.
%% @doc
%%      Stops connection with specific reason.
%% @end
stop_connection(Con, Reason, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
    etcp_connection:stop(Con, Reason, Timeout).
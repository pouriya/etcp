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
-module(etcp_transporter_ssl).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(etcp_transporter).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([init/1
        ,listen/2
        ,accept/2
        ,connect/3
        ,send/3
        ,recv/4
        ,check_message/3
        ,close/2
        ,shutdown/3
        ,controlling_process/3
        ,set_options/3
        ,peername/2
        ,get_options/2
        ,activate/2
        ,format_error/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {options % gen_tcp options
            ,metadata}).

-define(DEF_ACCEPTOR_ACCEPT_TIMEOUT, 3000).
-define(DEF_ACCEPTOR_HANDSHAKE_TIMEOUT, 3000).
-define(DEF_SOCKET_OPTIONS, []).
-define(DEF_CONNECT_TIMEOUT, 3000).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
init(etcp_types:transporter_options()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
init(Opts) when erlang:is_list(Opts) ->
    try {etcp_utils:get_value(connect_timeout
                             ,Opts
                             ,?DEF_CONNECT_TIMEOUT
                             ,fun etcp_utils:is_timeout/1)
        ,etcp_utils:get_value(accept_timeout
                             ,Opts
                             ,?DEF_ACCEPTOR_ACCEPT_TIMEOUT
                             ,fun etcp_utils:is_timeout/1)
        ,etcp_utils:get_value(handshake_timeout
                             ,Opts
                             ,?DEF_ACCEPTOR_HANDSHAKE_TIMEOUT
                             ,fun etcp_utils:is_timeout/1)} of
        {ConnectTimeout, AcceptTimeout, HandshakeTimeout} ->
            {ok, #?S{options = Opts -- [{connect_timeout, ConnectTimeout}
                                       ,{accept_timeout, AcceptTimeout}
                                       ,{handshake_timeout, HandshakeTimeout}]
                    ,metadata = #{accept_timeout => AcceptTimeout
                                 ,connect_timeout => ConnectTimeout
                                 ,handshake_timeout => HandshakeTimeout}}}
    catch
        _:{_, Rsn} when erlang:is_list(Rsn) ->
            {error, Rsn};
        _:Rsn ->
            {error, [{reason, Rsn}]}
    end.


-spec
listen(etcp_types:port_number(), etcp_types:transporter_state()) ->
    {'ok', {etcp_types:socket(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error().
listen(Port, #?S{options = Opts}=S) ->
    case ssl:listen(Port, filter_options(Opts, [])) of
        {ok, Sock} ->
            {ok, {Sock, S#?S{metadata = maps:remove(connect_timeout, S#?S.metadata)}}};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
accept(etcp_types:socket(), etcp_types:transporter_state()) ->
    {ok, {etcp_types:socket(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error().
accept(LSock, #?S{metadata = Metadata}=S) ->
    case ssl:transport_accept(LSock
                             ,maps:get(accept_timeout, Metadata, ?DEF_ACCEPTOR_ACCEPT_TIMEOUT)) of
        {ok, Sock} ->
            case ssl:ssl_accept(Sock
                               ,maps:get(handshake_timeout
                                        ,Metadata
                                        ,?DEF_ACCEPTOR_HANDSHAKE_TIMEOUT)) of
                ok ->
                    {ok, {Sock, S}};
                {error, Rsn} ->
                    {error, [{reason, Rsn}
                            ,{accept_timeout, maps:get(accept_timeout
                                                      ,Metadata
                                                      ,?DEF_ACCEPTOR_ACCEPT_TIMEOUT)}
                            ,{handshake_timeout, maps:get(handshake_timeout
                                                         ,Metadata
                                                         ,?DEF_ACCEPTOR_HANDSHAKE_TIMEOUT)}]}
            end;
        {error, Rsn} ->
            {error, [{reason, Rsn}, {accept_timeout, maps:get(accept_timeout
                                                             ,Metadata
                                                             ,?DEF_ACCEPTOR_ACCEPT_TIMEOUT)}]}
    end.


-spec
connect(etcp_types:host(), etcp_types:port_number(), etcp_types:transporter_state()) ->
    {'ok', {etcp_types:socket(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error().
connect(Host, Port, #?S{options = Opts, metadata = Metadata}=S) ->
    case ssl:connect(Host
                    ,Port
                    ,filter_options(Opts, [])
                    ,maps:get(connect_timeout, Metadata, ?DEF_CONNECT_TIMEOUT)) of
        {ok, Sock} ->
            {ok, {Sock, S#?S{metadata = maps:remove(handshake_timeout, maps:remove(accept_timeout, Metadata))}}};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
send(etcp_types:socket(), etcp_types:packet(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
send(Sock, Pkt, S) ->
    case ssl:send(Sock, Pkt) of
        ok ->
            {ok, S};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
recv(etcp_types:socket(), etcp_types:length(), timeout(), etcp_types:transporter_state()) ->
    {'ok', {etcp_types:packet(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error().
recv(Sock, Len, Timeout, S) ->
    case ssl:recv(Sock, Len, Timeout) of
        {ok, Pkt} ->
            {ok, {Pkt, S}};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
check_message(any(), etcp_types:socket(), etcp_types:transporter_state()) ->
    {'ok', {etcp_types:packet(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error()                       |
    'unknown'.
check_message({ssl, Sock, Pkt}, Sock, S) ->
    {ok, {Pkt, S}};
check_message({ssl_closed, Sock}, Sock, _) ->
    {error, [{reason, closed}]};
check_message({ssl_error, Sock, Rsn}, Sock, _) ->
    {error, [{reason, Rsn}]};
check_message(_, _, _) ->
    unknown.


-spec
close(etcp_types:socket(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
close(Sock, S) ->
    ssl:close(Sock),
    {ok, S}.


-spec
shutdown(etcp_types:socket(), etcp_types:shutdown_type(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
shutdown(Sock, Type, S) ->
    case ssl:shutdown(Sock, Type) of
        ok ->
            {ok, S};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
controlling_process(etcp_types:socket(), pid(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
controlling_process(Sock, Pid, S) ->
    case ssl:controlling_process(Sock, Pid) of
        ok ->
            {ok, S};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
activate(etcp_types:socket(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
activate(Sock, S) ->
    case ssl:setopts(Sock, [{active, true}]) of
        ok ->
            {ok, S};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
set_options(etcp_types:socket(), list(), etcp_types:transporter_state()) ->
    {'ok', etcp_types:transporter_state()} | etcp_types:transporter_callback_error().
set_options(Sock, SockOpts, S) ->
    case ssl:setopts(Sock, SockOpts) of
        ok ->
            {ok, S};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
peername(etcp_types:socket(), etcp_types:transporter_state()) ->
    {'ok', {etcp_types:address(), etcp_types:transporter_state()}} |
    etcp_types:transporter_callback_error().
peername(Sock, S) ->
    case ssl:peername(Sock) of
        {ok, PN} ->
            {ok, {PN, S}};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
get_options(etcp_types:socket(), etcp_types:transporter_state()) ->
    {'ok', {term(), etcp_types:transporter_state()}} | etcp_types:transporter_callback_error().
get_options(Sock, S) ->
    case ssl:getopts(Sock
                    ,[active
                     ,buffer
                     ,delay_send
                     ,deliver
                     ,dontroute
                     ,exit_on_close
                     ,header
                     ,high_msgq_watermark
                     ,high_watermark
                     ,keepalive
                     ,linger
                     ,low_msgq_watermark
                     ,low_watermark
                     ,mode
                     ,nodelay
                     ,packet
                     ,packet_size
                     ,priority
                     ,recbuf
                     ,reuseaddr
                     ,send_timeout
                     ,send_timeout_close
                     ,show_econnreset
                     ,sndbuf
%%                     ,ipv6_v6only %% Generates f**king error :-S
                     ,tos]) of
        {ok, Opts} ->
            {ok, {Opts, S}};
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.



-spec
format_error(any()) ->
    etcp_types:error_info().
format_error(closed) ->
    "Socket connection closed";
format_error(timeout) ->
    "Got timeout";
format_error(Rsn) ->
    ssl:format_error(Rsn).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

filter_options([{active, _}|Opts], Ret) ->
    filter_options(Opts, Ret);
filter_options([active|Opts], Ret) ->
    filter_options(Opts, Ret);
filter_options([Opt|Opts], Ret) ->
    filter_options(Opts, [Opt|Ret]);
filter_options(_, Ret) ->
    Ret.
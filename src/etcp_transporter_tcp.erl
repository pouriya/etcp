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
-module(etcp_transporter_tcp).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(etcp_transporter).
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([listen/2
        ,accept/2
        ,connect/4
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

-define(DEF_ACCEPTOR_ACCEPT_TIMEOUT, 3000).
-define(DEF_SOCKET_OPTIONS, []).
-define(DEF_CONNECT_TIMEOUT, 3000).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
listen(etcp_types:port_number(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:socket()} | etcp_types:transporter_callback_error().
listen(Port, Opts) ->
    case gen_tcp:listen(Port, [{active, false} | filter_options(Opts, [])]) of
        {ok, _}=Ok ->
            Ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
accept(etcp_types:socket(), etcp_types:transporter_options()) ->
    {ok, etcp_types:socket()} | etcp_types:transporter_callback_error().
accept(LSock, _) ->
    case gen_tcp:accept(LSock, ?DEF_ACCEPTOR_ACCEPT_TIMEOUT) of
        {ok, _}=Ok ->
            Ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
connect(etcp_types:host(), etcp_types:port_number(), etcp_types:transporter_options(), timeout()) ->
    {'ok', etcp_types:socket()} | etcp_types:transporter_callback_error().
connect(Host, Port, Opts, Timeout) ->
    case gen_tcp:connect(Host, Port, [{active, true} | filter_options(Opts, [])], Timeout) of
        {ok, _}=Ok ->
            Ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
send(etcp_types:socket(), etcp_types:packet(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().
send(Sock, Packet, _) ->
    case gen_tcp:send(Sock, Packet) of
        ok ->
            ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
recv(etcp_types:socket(), etcp_types:length(), timeout(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:packet()} | etcp_types:transporter_callback_error().
recv(Sock, Len, Timeout, _Opts) ->
    case gen_tcp:recv(Sock, Len, Timeout) of
        {ok, _}=Ok ->
            Ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
check_message(any(), etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:packet()} | etcp_types:transporter_callback_error() | 'unknown'.
check_message({tcp, Sock, Packet}, Sock, _) ->
    {ok, Packet};
check_message({tcp_closed, Sock}, Sock, _) ->
    {error, [{reason, closed}]};
check_message({tcp_error, Sock, Rsn}, Sock, _) ->
    {error, [{reason, Rsn}]};
check_message(_, _, _) ->
    unknown.


-spec
close(etcp_types:socket(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().
close(Sock, _Opts) ->
    gen_tcp:close(Sock),
    ok.


-spec
shutdown(etcp_types:socket(), etcp_types:shutdown_type(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().
shutdown(Sock, Type, _Opts) ->
    case gen_tcp:shutdown(Sock, Type) of
        ok ->
            ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
controlling_process(etcp_types:socket(), pid(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().
controlling_process(Sock, Pid, _Opts) ->
    case gen_tcp:controlling_process(Sock, Pid) of
        ok ->
            ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
activate(etcp_types:socket(), etcp_types:transporter_options()) ->
    ok | etcp_types:transporter_callback_error().
activate(Sock, _) ->
    case inet:setopts(Sock, [{active, true}]) of
        ok ->
            ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
set_options(etcp_types:socket(), list(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().
set_options(Sock, SockOpts, _Opts) ->
    case inet:setopts(Sock, SockOpts) of
        ok ->
            ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
peername(etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:address()} | etcp_types:transporter_callback_error().
peername(Sock, _Opts) ->
    case inet:peername(Sock) of
        {ok, _}=Ok ->
            Ok;
        {error, Rsn} ->
            {error, [{reason, Rsn}]}
    end.


-spec
get_options(etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', list()} | etcp_types:transporter_callback_error().
get_options(Sock, _Opts) ->
    case inet:getopts(Sock
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
                      ,tos
                      ,ipv6_v6only]) of
        {ok, _}=Ok ->
            Ok;
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
format_error(Reason) ->
    inet:format_error(Reason).

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
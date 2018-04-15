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
-module(etcp_transporter).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([listen/3
        ,accept/3
        ,connect/5
        ,send/4
        ,recv/5
        ,check_message/4
        ,close/3
        ,shutdown/4
        ,controlling_process/4
        ,activate/3
        ,set_options/4
        ,peername/3
        ,get_options/4
        ,format_error/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(stacktrace, {stacktrace, erlang:get_stacktrace()}).

%% -------------------------------------------------------------------------------------------------
%% Behavior info:

-callback
listen(etcp_types:port_number(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:socket()} | etcp_types:transporter_callback_error().


-callback
accept(etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:socket()} | etcp_types:transporter_callback_error().


-callback
connect(etcp_types:host(), etcp_types:port_number(), etcp_types:transporter_options(), timeout()) ->
    {'ok', etcp_types:socket()} | etcp_types:transporter_callback_error().


-callback
send(etcp_types:socket(), etcp_types:packet(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
recv(etcp_types:socket(), etcp_types:length(), timeout(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:packet()} | etcp_types:transporter_callback_error().


-callback
check_message(any(), etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:packet()} | etcp_types:transporter_callback_error() | 'unknown'.


-callback
close(etcp_types:socket(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
shutdown(etcp_types:socket(), etcp_types:shutdown_type(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
controlling_process(etcp_types:socket(), pid(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
set_options(etcp_types:socket(), any(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
peername(etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:address()} | etcp_types:transporter_callback_error().


-callback
get_options(etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', any()} | etcp_types:transporter_callback_error().


-callback
activate(etcp_types:socket(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:transporter_callback_error().


-callback
format_error(etcp_types:reason()) ->
    string().

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
listen(module(), etcp_types:port_number(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:socket()} | etcp_types:error().
%% @doc
%%      returns listen socket which can accepts incoming connections on
%%      given port.
%% @end
listen(TrMod, Port, Opts) ->
    try TrMod:listen(Port, Opts) of
        {ok, _}=Ok ->
            Ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_listen, format_error(TrMod, listen, [Port, Opts], ErrParams)}};
        Other ->
            {error, {socket_listen, [{return_value, Other}
                                    |wrap_mfa(TrMod, listen, [Port, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_listen, [{reason, Rsn}
                                    ,?stacktrace
                                    |wrap_mfa(TrMod, listen, [Port, Opts])]}}
    end.


-spec
accept(module(), etcp_types:socket(), etcp_types:transporter_options()) ->
    {ok, etcp_types:socket()} | etcp_types:error().
%% @doc
%%      accepts incoming connection and returns new connection socket.
%% @end
accept(TrMod, LSock, Opts) ->
    try TrMod:accept(LSock, Opts) of
        {ok, _}=Ok ->
            Ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_accept, format_error(TrMod, accept, [LSock, Opts], ErrParams)}};
        Other ->
            {error, {socket_accept, [{value, Other}
                                    |wrap_mfa(TrMod, accept, [LSock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_accept, [{rason, Rsn}
                                    ,?stacktrace
                                    |wrap_mfa(TrMod, accept, [LSock, Opts])]}}
    end.


-spec
connect(module()
       , etcp_types:host()
       , etcp_types:port_number()
       , etcp_types:transporter_options()
       ,timeout()) ->
    {'ok', etcp_types:socket()} | etcp_types:error().
%% @doc
%%      makes a connection to remote host and port.
%% @end
connect(TrMod, Host, Port, Opts, Timeout) ->
    try TrMod:connect(Host, Port, Opts, Timeout) of
        {ok, _}=Ok ->
            Ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_connect, format_error(TrMod, connect, [Host, Port, Opts], ErrParams)}};
        Other ->
            {error, {socket_connect, [{value, Other}
                                     |wrap_mfa(TrMod, connect, [Host, Port, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_connect, [{reason, Rsn}
                                     ,?stacktrace
                                     |wrap_mfa(TrMod, connect, [Host, Port, Opts])]}}
    end.


-spec
send(module()
    , etcp_types:socket()
    , etcp_types:packet()
    , etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      sends packet through socket.
%% @end
send(TrMod, Sock, Pkt, Opts) ->
    try TrMod:send(Sock, Pkt, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_send, format_error(TrMod, send, [Sock, Pkt, Opts], ErrParams)}};
        Other ->
            {error, {socket_send, [{value, Other}
                                  |wrap_mfa(TrMod, send, [Sock, Pkt, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_send, [{reason, Rsn}
                                  ,?stacktrace
                                  |wrap_mfa(TrMod, send, [Sock, Pkt, Opts])]}}
    end.


-spec
recv(module()
    , etcp_types:socket()
    , etcp_types:length()
    ,timeout()
    , etcp_types:transporter_options()) ->
    {ok, etcp_types:packet()} | etcp_types:error().
%% @doc
%%      Receives data from passive socket.
%% @end
recv(TrMod, Sock, Len, Timeout, Opts) ->
    try TrMod:recv(Sock, Len, Timeout, Opts) of
        {ok, _}=Ok ->
            Ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_recv, format_error(TrMod, recv, [Sock, Len, Timeout], ErrParams)}};
        Other ->
            {error, {socket_recv, [{retruned_value, Other}
                                  ,wrap_mfa(TrMod, recv, [Sock, Len, Timeout])]}}
    catch
        _:Rsn ->
            {error, {socket_recv, [{reason, Rsn}
                                  ,?stacktrace
                                  |wrap_mfa(TrMod, recv, [Sock, Len, Timeout])]}}
    end.


-spec
check_message(module(), any(), etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:packet()} | etcp_types:error() | 'unknown'.
%% @doc
%%      determines that the term is message like messages of gen_tcp
%%      and ssl sockets in active mode or not.
%% @end
check_message(TrMod, Msg, Sock, Opts) ->
    try TrMod:check_message(Msg, Sock, Opts) of
        {ok, _}=Ok ->
            Ok;
        unknown ->
            unknown;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_check_message, format_error(TrMod
                                                       ,check_message
                                                       ,[Msg, Sock, Opts]
                                                       ,ErrParams)}};
        Other ->
            {error, {socket_check_message, [{value, Other}
                                           |wrap_mfa(TrMod, check_message, [Msg, Sock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_check_message, [{reason, Rsn}
                                           ,?stacktrace
                                           |wrap_mfa(TrMod, check_message, [Msg, Sock, Opts])]}}
    end.


-spec
close(module(), etcp_types:socket(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      Closes an open socket.
%% @end
close(TrMod, Sock, Opts) ->
    try TrMod:close(Sock, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_close, format_error(TrMod, close, [Sock, Opts], ErrParams)}};
        Other ->
            {error, {socket_close, [{value, Other}
                                   |wrap_mfa(TrMod, close, [Sock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_close, [{reason, Rsn}
                                   ,?stacktrace
                                   |wrap_mfa(TrMod, close, [Sock, Opts])]}}
    end.


-spec
shutdown(module()
        , etcp_types:socket()
        , etcp_types:shutdown_type()
        , etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      closes read or write or read/write of socket.
%% @end
shutdown(TrMod, Sock, Type, Opts) ->
    try TrMod:shutdown(Sock, Type, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_shutdown, format_error(TrMod
                                                  ,shutdown
                                                  ,[Sock, Type, Opts]
                                                  ,ErrParams)}};
        Other ->
            {error, {socket_shutdown, [{value, Other}
                                      |wrap_mfa(TrMod, shutdown, [Sock, Type, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_shutdown, [{reason, Rsn}
                                      ,?stacktrace
                                      |wrap_mfa(TrMod, shutdown, [Sock, Type, Opts])]}}
    end.


-spec
controlling_process(module(), etcp_types:socket(), pid(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      changes the receiver of socket messages.
%% @end
controlling_process(TrMod, Sock, Pid, Opts) ->
    try TrMod:controlling_process(Sock, Pid, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_controlling_process, format_error(TrMod
                                                             ,controlling_process
                                                             ,[Sock, Pid, Opts]
                                                             ,ErrParams)}};
        Other ->
            {error, {socket_controlling_process, [{value, Other}
                                                 |wrap_mfa(TrMod
                                                          ,controlling_process
                                                          ,[Sock, Pid, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_controlling_process, [{reason, Rsn}
                                                 ,?stacktrace
                                                 |wrap_mfa(TrMod
                                                          ,controlling_process
                                                          ,[Sock, Pid, Opts])]}}
    end.


-spec
activate(module(), etcp_types:socket(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      returns activity flag of socket.
%% @end
activate(TrMod, Sock, Opts) ->
    try TrMod:activate(Sock, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_activate, format_error(TrMod, activity, [Sock, Opts], ErrParams)}};
        Other ->
            {error, {socket_activate, [{value, Other}
                                      |wrap_mfa(TrMod, activity, [Sock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_activate, [{reason, Rsn}
                                      ,?stacktrace
                                      |wrap_mfa(TrMod, activity, [Sock, Opts])]}}
    end.


-spec
set_options(module(), etcp_types:socket(), list(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      Changes options of socket.
%% @end
set_options(TrMod, Sock, SockOpts, Opts) ->
    try TrMod:set_options(Sock, SockOpts, Opts) of
        ok ->
            ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_set_options, format_error(TrMod
                                                     ,set_options
                                                     ,[Sock, SockOpts, Opts]
                                                     ,ErrParams)}};
        Other ->
            {error, {socket_set_options, [{value, Other}
                                         |wrap_mfa(TrMod, set_options, [Sock, SockOpts, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_set_options, [{reason, Rsn}
                                         ,?stacktrace
                                         |wrap_mfa(TrMod, set_options, [Sock, SockOpts, Opts])]}}
    end.


-spec
peername(module(), etcp_types:socket(), etcp_types:transporter_options()) ->
    {'ok', etcp_types:address()} | etcp_types:error().
%% @doc
%%      Fetchs address of socket.
%% @end
peername(TrMod, Sock, Opts) ->
    try TrMod:peername(Sock, Opts) of
        {ok, {IP, Port}}=Ok when (erlang:is_tuple(IP) orelse erlang:is_list(IP)) andalso
                                 erlang:is_integer(Port) ->
            Ok;
        {ok, _} ->
            {ok, {undefined, undefined}};
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_peername, format_error(TrMod, peername, [Sock, Opts], ErrParams)}};
        Other ->
            {error, {socket_peername, [{value, Other}
                                      |wrap_mfa(TrMod, peername, [Sock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_peername, [{reason, Rsn}
                                      ,?stacktrace
                                      |wrap_mfa(TrMod, peername, [Sock, Opts])]}}
    end.


-spec
get_options(module(), etcp_types:socket(), list(), etcp_types:transporter_options()) ->
    'ok' | etcp_types:error().
%% @doc
%%      Gets options of socket.
%% @end
get_options(TrMod, Sock, OptList, Opts) ->
    try TrMod:get_options(Sock, OptList, Opts) of
        {ok, _}=Ok ->
            Ok;
        {error, ErrParams} when erlang:is_list(ErrParams) ->
            {error, {socket_get_options, format_error(TrMod
                                                     ,get_options
                                                     ,[Sock, Opts]
                                                     ,ErrParams)}};
        Other ->
            {error, {socket_get_options, [{value, Other}
                                         |wrap_mfa(TrMod, get_options, [Sock, Opts])]}}
    catch
        _:Rsn ->
            {error, {socket_get_options, [{reason, Rsn}
                                         ,?stacktrace
                                         |wrap_mfa(TrMod, get_options, [Sock, Opts])]}}
    end.


-spec
format_error(module(), any()) ->
    [] | [{'info', string()}].
%% @doc
%%      Returns understandable string or binary about error.
%% @end
format_error(TrMod, Rsn) ->
    try TrMod:format_error(Rsn) of
        Info when erlang:is_list(Info) ->
            [{info, Info}];
        _ ->
            []
    catch
        _:_ ->
            []
    end.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

format_error(TrMod, Func, Args, [{reason, Rsn}|_]=ErrParams) ->
    ErrParams ++ format_error(TrMod, Rsn) ++ wrap_mfa(TrMod, Func, Args);
format_error(TrMod, Func, Args, ErrParams) ->
    ErrParams ++ wrap_mfa(TrMod, Func, Args).


wrap_mfa(Mod, Func, Args) ->
    [{module, Mod}, {function, Func}, {arguments, Args}].
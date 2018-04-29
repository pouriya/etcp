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
-module(etcp_connection).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/4
        ,start_link/5
        ,start_link/6
        ,send_sync/2
        ,send_sync/3
        ,send_async/2
        ,stop/1
        ,stop/2
        ,stop/3]).

%% 'proc_lib' callback:
-export([init/1]).

%% 'sys' callbacks:
-export([system_terminate/4
        ,system_replace_state/2
        ,system_get_state/1
        ,system_continue/3
        ,system_code_change/4]).

-export([loop/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(?S, {name
            ,data
            ,module
            ,hibernate
            ,transporter
            ,transporter_state
            ,socket
            ,callback
            ,message
            ,error_logger
            ,timeout}).


%% Dependencies:
%%  #etcp_metadata{}
-include("internal/etcp_metadata.hrl").
-define(ETCP_MD, etcp_metadata).

-define(DEF_TERMINATE_TIMEOUT, 5000).
-define(GEN_CALL_TAG, '$gen_call').
-define(GEN_CAST_TAG, '$gen_cast').
-define(GEN_EVENT_TAG, '$gen_event').
-define(GEN_ETCP_TAG, '$gen_etcp').
-define(DEF_LENGHT, 0).
-define(DEF_TIMEOUT, infinity).
-define(DEF_SRTIMEOUT, infinity).
-define(DEF_START_OPTS, #{}).
-define(DEF_PROC_START_OPTS, []).
-define(DEF_TRANSPORT_OPTIONS, []).
-define(DEF_SSL_FLAG, false).
-define(DEF_CONNECT_TIMEOUT, 3000).
-define(DEF_DEBUG, []).
-define(DEF_TRANSPORT_MODULE, 'etcp_transporter_tcp').
-define(DEF_TERMINATE_LOG, 'true').
-define(DEF_CALL_ERROR_LOGGER, 'true').
-define(EMPTY_DEBUG, []).


-include("internal/etcp_guard.hrl").

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
start_link(module(), term(), etcp_types:host(), etcp_types:port_number()) ->
    etcp_types:start_return().
%% @doc
%%      starts and links a socket connection handler process.
%% @end
start_link(Mod, InitArg, Host, Port) when erlang:is_atom(Mod) andalso
                                          erlang:is_integer(Port) ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[{erlang:self()
                         ,erlang:self()
                         ,Mod
                         ,InitArg
                         ,Host
                         ,Port
                         ,?DEF_START_OPTS}]).


-spec
start_link(module() | etcp_types:register_name()
          ,term() | module()
          ,etcp_types:host()| term() | etcp_types:start_options()
          ,etcp_types:port_number() | etcp_types:host()
          ,etcp_types:options() | etcp_types:port_number()) ->
    etcp_types:start_return().
start_link(Mod, InitArg, Host, Port, Opts) when erlang:is_atom(Mod) andalso
                                                erlang:is_integer(Port) andalso
                                                erlang:is_map(Opts) ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[{erlang:self(), erlang:self(), Mod, InitArg, Host, Port, Opts}]);
start_link(Name, Mod, InitArg, Host, Port) when erlang:is_tuple(Name) andalso
                                                erlang:is_atom(Mod) andalso
                                                erlang:is_integer(Port) ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[{erlang:self(), Name, Mod, InitArg, Host, Port, ?DEF_START_OPTS}]).


-spec
start_link(etcp_types:register_name()
          ,module()
          ,term()
          , etcp_types:host()
          , etcp_types:port_number()
          , etcp_types:options()) ->
    etcp_types:start_return() | {'ok', pid(), etcp_types:transporter_state()}.
start_link(Name, Mod, InitArg, Host, Port, Opts) when erlang:is_tuple(Name) andalso
                                                      erlang:is_atom(Mod) andalso
                                                      erlang:is_integer(Port) andalso
                                                      erlang:is_map(Opts) ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[{erlang:self(), Name, Mod, InitArg, Host, Port, Opts}]);
start_link(Mod, InitArg, Opts, TrMod, LSock, TrState) when erlang:is_atom(Mod) andalso
                                                           erlang:is_map(Opts) andalso
                                                           erlang:is_atom(TrMod) ->
    proc_lib:start(?MODULE
                  ,init
                  ,[{acceptor, erlang:self(), Mod, InitArg, Opts, TrMod, LSock, TrState}]).


-spec
send_sync(etcp_types:name(), etcp_types:packet()) ->
    'ok' | etcp_types:error().
send_sync(Con, Packet) when ?is_proc_ref(Con) ->
    case catch gen:call(Con, ?GEN_ETCP_TAG, {send, Packet}) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({Reason, [{module, ?MODULE}
                                 ,{function, send_sync}
                                 ,{arguments, [Con, Packet]}]})
    end.


-spec
send_sync(etcp_types:name(), etcp_types:packet(), timeout()) ->
    'ok' | etcp_types:error().
send_sync(Con, Packet, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
    case catch gen:call(Con, ?GEN_ETCP_TAG, {send, Packet}, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({Reason, [{module, ?MODULE}
                                 ,{function, send_sync}
                                 ,{arguments, [Con, Packet, Timeout]}]})
    end.


-spec
send_async(etcp_types:name(), etcp_types:packet()) ->
    'ok'.
send_async(Con, Packet) when ?is_proc_ref(Con) ->
    catch Con ! {?GEN_ETCP_TAG, {send, Packet}},
    ok.


-spec
stop(etcp_types:name()) ->
    ok.
stop(Con) when ?is_proc_ref(Con) ->
    proc_lib:stop(Con, normal, ?DEF_TERMINATE_TIMEOUT).


-spec
stop(etcp_types:name(), etcp_type:reason()) ->
    ok.
stop(Con, Reason) when ?is_proc_ref(Con) ->
    proc_lib:stop(Con, Reason, ?DEF_TERMINATE_TIMEOUT).


-spec
stop(etcp_types:name(), etcp_type:reason(), timeout()) ->
  ok.
stop(Con, Reason, Timeout) when ?is_proc_ref(Con) andalso ?is_timeout(Timeout) ->
  proc_lib:stop(Con, Reason, Timeout).

%% -------------------------------------------------------------------------------------------------
%% 'gen' callback:

init({Parent, Name, Mod, InitArg, Host, Port, Opts}) ->
    Name2 =
        if
            Parent =:= Name ->
                erlang:self();
            true ->
                Name
        end,
    case register(Name2) of
        {ok, Name3} ->
            TrMod = etcp_utils:get_value(transporter
                                        ,Opts
                                        ,?DEF_TRANSPORT_MODULE
                                        ,fun erlang:is_atom/1),
            TrOpts = etcp_utils:get_value(transporter_options
                                         ,Opts
                                         ,?DEF_TRANSPORT_OPTIONS
                                         ,fun erlang:is_list/1),
            case etcp_transporter:init(TrMod, TrOpts) of
                {ok, TrState} ->
                    case etcp_transporter:connect(TrMod, Host, Port, TrState) of
                        {ok, {Sock, TrState2}} ->
                            DbgOpts = etcp_utils:get_value(connection_debug
                                                          ,Opts
                                                          ,?DEF_DEBUG
                                                          ,fun erlang:is_list/1),
                            CallErrLogger = etcp_utils:get_value(call_error_logger
                                                                ,Opts
                                                                ,?DEF_CALL_ERROR_LOGGER
                                                                ,fun erlang:is_boolean/1),
                            Dbg = etcp_utils:debug_options(?MODULE, Name, DbgOpts),
                            State = #?S{name = Name3
                                       ,module = Mod
                                       ,data = InitArg
                                       ,transporter = TrMod
                                       ,socket = Sock
                                       ,transporter_state = TrState2
                                       ,error_logger = CallErrLogger
                                       ,hibernate = false
                                       ,timeout = undefined},
                            MetaData = etcp_metadata:wrap(TrMod, Sock, TrState2),
                            case run_callback2(Dbg
                                              ,State
                                              ,Mod
                                              ,connection_init
                                              ,[InitArg, MetaData]) of
                                {ok, Dbg2, State2} ->
                                    _ = proc_lib:init_ack(Parent, {ok, erlang:self()}),
                                    loop(Parent, Dbg2, State2);
                                {close, _Dbg2, _State2} ->
                                    _ = proc_lib:init_ack(Parent, {error, normal}),
                                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                                    erlang:exit(normal);
                                {stop, _, _, Rsn} ->
                                    _ = proc_lib:init_ack(Parent, {error, Rsn}),
                                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                                    erlang:exit(Rsn);
                                {error, {return, [{value, ignore}|_]}} ->
                                    _ = proc_lib:init_ack(Parent, ignore),
                                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                                    erlang:exit(normal);
                                {error, Rsn} ->
                                    _ = proc_lib:init_ack(Parent, {error, Rsn}),
                                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                                    erlang:exit(Rsn)
                            end;
                        {error, Rsn} ->
                            _ = proc_lib:init_ack(Parent, {error, Rsn}),
                            erlang:exit(Rsn)
                    end;
                {error, Rsn} ->
                    _ = proc_lib:init_ack(Parent, {error, Rsn}),
                    erlang:exit(Rsn)
            end;
        {error, Rsn} ->
            _ = proc_lib:init_ack(Parent, {error, Rsn}),
            erlang:exit(Rsn)
    end;

init({acceptor, Parent, Mod, InitArg, Opts, TrMod, LSock, TrState}) ->
    Name = erlang:self(),
    case etcp_acceptor:accept(TrMod, LSock, TrState) of
        {ok, {Sock, TrState2}} ->
            _ = proc_lib:init_ack(Parent, {ok, Name, TrState2}),
            DbgOpts = etcp_utils:get_value(connection_debug
                                          ,Opts
                                          ,?DEF_DEBUG
                                          ,fun erlang:is_list/1),
            CallErrLogger = etcp_utils:get_value(call_error_logger
                                                ,Opts
                                                ,?DEF_CALL_ERROR_LOGGER
                                                ,fun erlang:is_boolean/1),
            Dbg = etcp_utils:debug_options(?MODULE, Name, DbgOpts),
            State = #?S{name = Name
                       ,module = Mod
                       ,data = InitArg
                       ,transporter = TrMod
                       ,socket = Sock
                       ,transporter_state = TrState2
                       ,error_logger = CallErrLogger
                       ,hibernate = false
                       ,timeout = undefined},
            MetaData = etcp_metadata:wrap(TrMod, Sock, TrState2),
            case run_callback2(Dbg, State, Mod, connection_init, [InitArg, MetaData]) of
                {ok, Dbg2, State2} ->
                    io:format("salaaaaaaaaaaaaaaaaaaaaam"),
                    loop(Parent, Dbg2, State2);
                {close, _Dbg2, _State2} ->
                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                    erlang:exit(normal);
                {stop, _, _, Rsn} ->
                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                    erlang:exit(Rsn);
                {error, {return, [{value, ignore}|_]}} ->
                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                    erlang:exit(normal);
                {error, Rsn} ->
                    _ = etcp_transporter:close(TrMod, Sock, Opts),
                    erlang:exit(Rsn)
            end;
        {error, _}=Err ->
            _ = proc_lib:init_ack(Parent, Err),
            erlang:exit(normal)
    end.

%% -------------------------------------------------------------------------------------------------
%% 'sys' callbacks:

%% @hidden
system_code_change(#?S{module = Mod, data = Data}=State, _Module, OldVsn, Extra) ->
    case Mod:code_change(OldVsn, Data, Extra) of
        {ok, Data2} ->
            {ok, State#?S{data = Data2}};
        Other ->
            Other
    end.


%% @hidden
system_continue(Parent, Dbg, State) ->
    ?MODULE:loop(Parent, Dbg, State).


%% @hidden
system_get_state(#?S{data=Data}) ->
    {ok, Data}.


%% @hidden
system_replace_state(StateFun, #?S{data=Data}=State) ->
    Data2 = StateFun(Data),
    {ok, Data2, State#?S{data=Data2}}.


%% @hidden
system_terminate(Reason, _, Dbg, State) ->
    terminate(Dbg, State, Reason).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

loop(Parent, Dbg, #?S{hibernate = false, timeout = undefined, name = Name}=State) ->
    Msg =
        receive
            Msg2 ->
                Msg2
        end,
    process_message(Parent, debug(Name, Dbg, {in, Msg}), State#?S{message = Msg}, Msg);
loop(Parent, Dbg, #?S{hibernate = false, timeout = Timeout, name = Name}=State) ->
    receive
        Msg ->
            process_message(Parent, debug(Name, Dbg, {in, Msg}), State#?S{message = Msg}, Msg)
    after Timeout ->
        process_message(Parent, debug(Name, Dbg, timeout), State, timeout)
    end;
loop(Parent, Dbg, State) ->
    proc_lib:hibernate(?MODULE, loop, [Parent, Dbg, State#?S{hibernate = false}]).


process_message(Parent, Dbg, #?S{name=Name}=State, {?GEN_ETCP_TAG, From, Req}=Msg) ->
    {Dbg2, State2} = process_sync_request(debug(Name, Dbg, Msg), State, From, Req),
    ?MODULE:loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, #?S{name=Name}=State, {?GEN_ETCP_TAG, Req}=Msg) ->
    {Dbg2, State2} = process_async_request(debug(Name, Dbg, Msg), State, Req),
    ?MODULE:loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, #?S{name= Name}=State, {?GEN_CALL_TAG, From, Req}=Msg) ->
    {Dbg2, State2} = run_callback(debug(Name, Dbg, Msg), State, handle_call, {Req, From}),
    ?MODULE:loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, #?S{name= Name}=State, {?GEN_CAST_TAG, Msg2}=Msg) ->
    {Dbg2, State2} = run_callback(debug(Name, Dbg, Msg), State, handle_cast, {Msg2}),
    ?MODULE:loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, #?S{name = Name}=State, {system, From, Msg}=SysMsg) ->
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, debug(Name, Dbg, SysMsg), State);
process_message(Parent, Dbg, State, {'EXIT', Parent, Rsn}) ->
    terminate(Dbg, State, Rsn);
process_message(Parent
               ,Dbg
               ,#?S{name = Name
                   ,transporter = TrMod
                   ,socket = Sock
                   ,transporter_state = TrState}=State
               ,Msg) ->
    case etcp_transporter:check_message(TrMod, Msg, Sock, TrState) of
        {ok, {Pkt, TrState2}} ->
            {Dbg2, State2} = run_callback(debug(Name, Dbg, {socket_in, Pkt})
                                         ,State#?S{transporter_state = TrState2}
                                         ,handle_packet
                                         ,{Pkt}),
            ?MODULE:loop(Parent, Dbg2, State2);
        unknown ->
            {Dbg2, State2} = run_callback(Dbg, State, handle_info, {Msg}),
            ?MODULE:loop(Parent, Dbg2, State2);
        {error, {socket_check_message, [{reason, closed}|_]}} ->
            {Dbg2, State2} = run_callback(Dbg, State, handle_disconnect, {}),
            terminate(Dbg2, State2, normal);
        {error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


process_sync_request(Dbg
                    ,#?S{name = Name
                        ,transporter = TrMod
                        ,socket = Sock
                        ,transporter_state = TrState}=State
                    ,From
                    ,{send, Pkt}) ->
    case socket_send(TrMod, Sock, Name,  Dbg, Pkt, TrState) of
        {ok, {Dbg2, TrState2}} ->
            {reply(Name, Dbg2, From, ok), State#?S{transporter_state = TrState2}};
        {error, Rsn}=Err ->
            terminate(reply(Name, Dbg, From, Err), State, Rsn)
    end;
process_sync_request(Dbg, #?S{name = Name}=State, From, Other) ->
    {reply(Name, Dbg, From, {error, {unknown, [{request, Other}]}}), State}.


process_async_request(Dbg
                     ,#?S{name = Name
                         ,transporter = TrMod
                         ,socket = Sock
                         ,transporter_state = TrState}=State
                     ,{send, Pkt}) ->
    case socket_send(TrMod, Sock, Name,  Dbg, Pkt, TrState) of
        {ok, {Dbg2, TrState2}} ->
            {Dbg2, State#?S{transporter_state = TrState2}};
        {error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end;
process_async_request(Dbg, State, Other) ->
    terminate(Dbg, State, {unknown, [{request, Other}]}).


reply(Name, Dbg, {Pid, Tag}=Client, Msg) ->
    catch Pid ! {Tag, Msg},
    debug(Name, Dbg, {out, Client, Msg});
reply(Name, Dbg, Pid, Msg) ->
    catch Pid ! Msg,
    debug(Name, Dbg, {out, Pid, Msg}).


run_callback(Dbg
            ,#?S{module = Mod
                ,data= Data
                ,transporter = TrMod
                ,socket = Sock
                ,transporter_state = TrState}=State
            ,Callback
            ,Args) ->
    MetaData = etcp_metadata:wrap(TrMod, Sock, TrState),
    Args2 =
        case Args of
            {} ->
                [Data, MetaData];
            {Arg} ->
                [Arg, Data, MetaData];
            {Arg1, Arg2} ->
                [Arg1, Arg2, Data, MetaData]
        end,
    case run_callback2(Dbg, State, Mod, Callback, Args2) of
        {ok, Dbg2, State2} ->
            {Dbg2, State2};
        {close, Dbg2, State2} ->
            terminate(Dbg2, State2, normal);
        {stop, Dbg2, State2, Reason} ->
            terminate(Dbg2, State2, Reason);
        {error, Reason} ->
            terminate(Dbg, State, Reason)
    end.


run_callback2(Dbg, State, Mod, Func, Args) ->
    Ret =
        try erlang:apply(Mod, Func, Args) of
            ok ->
                {ok, Dbg, State#?S{callback = Func}};
            {ok, Opts} ->
                get_options(Dbg, State, Opts);
            close ->
                {close, Dbg, State#?S{callback = Func}};
            {close, Opts} ->
                case get_options(Dbg, State, Opts) of
                    {ok, Dbg2, State2}  ->
                        {close, Dbg2, State2#?S{callback = Func}};
                    {error, _}=Err ->
                        Err
                end;
            {stop, Rsn} ->
                {stop, Dbg, State#?S{callback = Func}, Rsn};
            {stop, Rsn, Opts} ->
                case get_options(Dbg, State, Opts) of
                    {ok, Dbg2, State2}  ->
                        {stop, Dbg2, State2#?S{callback = Func}, Rsn};
                    {error, _}=Err ->
                        Err
                end;
            Other ->
                {error, {return, [{value, Other}]}}
        catch
            _:Rsn ->
                {error, {crash, [{reason, Rsn}, {stacktrace, erlang:get_stacktrace()}]}}
        end,
    case Ret of
        {error, {Rsn2, ErrParams}} ->
            {error, {Rsn2, ErrParams ++ [{module, Mod}, {function, Func}, {arguments, Args}]}};
        _ ->
            Ret
    end.


get_options(Dbg, State, [H|T]) when not erlang:is_list(T) ->
    %% Fix improper list:
    get_options(Dbg, State, [H,T]);
get_options(Dbg, State, [{state, Data} | Opts]) ->
    get_options(Dbg, State#?S{data = Data}, Opts);
get_options(Dbg
           ,#?S{name = Name
               ,transporter = TrMod
               ,socket = Sock
               ,transporter_state = TrState}=State
           ,[{packet, Pkt} | Opts]) ->
    case socket_send(TrMod, Sock, Name, Dbg, Pkt, TrState) of
        {ok, {Dbg2, TrState2}} ->
            get_options(Dbg2, State#?S{transporter_state = TrState2}, Opts);
        {error, _}=Err ->
            Err
    end;
get_options(Dbg, #?S{name = Name}=State, [{reply, {To, Msg}}|Opts]) ->
    get_options(reply(Name, Dbg, To, Msg), State, Opts);
get_options(Dbg, State, [{timeout, Timeout}|Opts]) when (erlang:is_integer(Timeout) andalso
                                                         Timeout >= 0 andalso
                                                         % max value for 'receive' statement
                                                         Timeout =< 4294967295) orelse
                                                        Timeout =:= infinity ->
    get_options(Dbg, State#?S{timeout = Timeout}, Opts);
get_options(Dbg, State, [{hibernate, HibernateFlag}|Opts]) when erlang:is_boolean(HibernateFlag) ->
    get_options(Dbg, State#?S{hibernate = HibernateFlag}, Opts);
get_options(Dbg, State, []) ->
    {ok, Dbg, State};
get_options(Dbg
           ,State
           ,[{transporter, TrMod} | Opts]) when erlang:is_atom(TrMod) ->
    get_options(Dbg, State#?S{transporter = TrMod}, Opts);
get_options(Dbg, State, [{socket, Sock} | Opts]) ->
    get_options(Dbg, State#?S{socket = Sock}, Opts);
get_options(Dbg, State, [{transporter_state, TrState} | Opts]) ->
    get_options(Dbg, State#?S{transporter_state = TrState}, Opts);
%% Catch clauses:
get_options(_Dbg, _State, [{transporter, TrMod}|_Opts]) ->
    {error, {option_value, [{transporter, TrMod}]}};
get_options(_Dbg, _State, [{timeout, Timeout}|_Opts]) ->
    {error, {option_value, [{timeout, Timeout}]}};
get_options(_Dbg, _State, [{reply, To_Msg}|_Opts]) ->
    {error, {option_value, [{reply, To_Msg}]}};
get_options(_Dbg, _State, [{hibernate, HibernateFlag}|_Opts]) ->
    {error, {option_value, [{hibernate, HibernateFlag}]}};
get_options(_Dbg, _State, [{Key, Val}|_Opts]) ->
    {error, {option_value, [{key, Key}, {value, Val}]}};
get_options(_Dbg, _State, [Opt|_Opts]) ->
    {error, {option_value, [{option, Opt}]}};
get_options(_Dbg, _State ,Opts) ->
    {error, {options_value, [{options, Opts}]}}.


socket_send(TrMod, Sock, Name,  Dbg, Pkt, TrState) ->
    case etcp_transporter:send(TrMod, Sock, Pkt, TrState) of
        {ok, TrState2} ->
            {ok, {debug(Name, Dbg, {socket_out, Pkt}), TrState2}};
        {error, _}=Err ->
            Err
    end.


terminate(Dbg
         ,#?S{module = Mod
             ,data = Data
             ,name = Name
             ,transporter = TrMod
             ,socket = Sock
             ,transporter_state = TrState
             ,error_logger = ShouldLog
             ,callback = Func
             ,message = Msg}
         ,Rsn) ->
    Metadata = etcp_metadata:wrap(TrMod, Sock, TrState),
    Rsn2 =
        try Mod:terminate(Rsn, Data, Metadata) of
            {error, Rsn3} ->
                Rsn3;
            _ ->
                Rsn
        catch
            _:Rsn3 ->
                [Rsn, {error, {crash, [{reason, Rsn3}
                                      ,{stacktrace, erlang:get_stacktrace()}
                                      ,{module, Mod}
                                      ,{function, terminate}
                                      ,{arguments, [Rsn, Data, Metadata]}]}}]
        end,
    if
        ShouldLog ->
            error_logger:format("** ETCP connection ~p terminating \n"
                                "** Reason for termination == ~p\n"
                                "** Last callback == ~p\n"
                                "** Last message == ~p\n"
                                "** State == ~p\n"
                               ,[Name, Rsn2, Func, Msg, Data]);
        true ->
            ok
    end,
    _ = etcp_transporter:close(TrMod, Sock, TrState),
    sys:print_log(Dbg),
    erlang:exit(Rsn2).


debug(_, [], _) ->
    [];
debug(Name, Dbg, Event) ->
    sys:handle_debug(Dbg, fun handle_debug/3, Name, Event).


handle_debug(IODev, {socket_in, Packet}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p got packet ~p\n", [Name, Packet]);
handle_debug(IODev, {socket_out, Packet}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p sent packet ~p\n", [Name, Packet]);
handle_debug(IODev, {in, Msg}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p got message ~p\n", [Name, Msg]);
handle_debug(IODev, {out, {Pid, Tag}, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* ETCP connection ~p sent ~p to ~p with tag ~p\n"
             ,[Name, Msg, Pid, Tag]);
handle_debug(IODev, {out, Pid, Msg}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p sent ~p to ~p\n", [Name, Msg, Pid]);
handle_debug(IODev, {?GEN_CALL_TAG, {Pid, Tag}, Req}, Name) ->
    io:format(IODev
             ,"*DBG* ETCP connection ~p got call ~p from ~p with tag ~p\n"
             ,[Name, Req, Pid, Tag]);
handle_debug(IODev, {?GEN_CAST_TAG, Msg}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p got cast ~p\n", [Name, Msg]);
handle_debug(IODev, {?GEN_EVENT_TAG, Event}, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p got event ~p\n", [Name, Event]);
handle_debug(IODev, {?GEN_ETCP_TAG, {Pid, Tag}, {send, Pkt}}, Name) ->
    io:format(IODev
             ,"*DBG* ETCP connection ~p got synchronous request for sending packet ~p from ~p with t"
              "ag ~p\n"
             ,[Name, Pkt, Pid, Tag]);
handle_debug(IODev, {?GEN_ETCP_TAG, {send, Pkt}}, Name) ->
    io:format(IODev
             ,"*DBG* ETCP connection ~p got asynchronous requestfor sending packet ~p\n"
             ,[Name, Pkt]);
handle_debug(IODev, {system, {Pid, _Tag}, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* ETCP connection ~p got system message '~p' from '~p'"
             ,[Name, Msg, Pid]);
handle_debug(IODev, Event, Name) ->
    io:format(IODev, "*DBG* ETCP connection ~p got debug event ~p\n", [Name, Event]).


register(Name) when erlang:is_pid(Name) ->
    {ok, Name};
register({local, Name}) ->
    try
        erlang:register(Name, erlang:self()),
        {ok, Name}
    catch
        _:_ ->
            {error, {register_name, [{name, {local, Name}}]}}
    end;
register({global, Name}) ->
    try global:register_name(Name, erlang:self()) of
        yes ->
            {ok, {global, Name}};
        _ -> % no
            Rsn =
                try global:whereis_name(Name) of
                    Pid ->
                        [{reason, already_registered}, {pid, Pid}, {name, {global, Name}}]
                catch
                    _:_ ->
                        [{name, {global, Name}}]
                end,
            {error, {register_name, Rsn}}
    catch
        _:_ ->
            {error, {register_name, [{name, {global, Name}}]}}
    end;
register({via, Mod, Name}) ->
    try Mod:register_name(Name, erlang:self()) of
        yes ->
            {ok, {via, Mod, Name}};
        _ -> % no
            Rsn =
                try Mod:whereis_name(Name) of
                    Pid ->
                        [{reason, already_registered}, {pid, Pid}, {name, {via, Mod, Name}}]
                catch
                    _:_ ->
                        [{name, {via, Mod, Name}}]
                end,
            {error, {register_name, Rsn}}
    catch
        _:_ ->
            {error, {register_name, [{name, {via, Mod, Name}}]}}
    end;
register(Arg) ->
    {error, {register_name, [{reason, unknown}, {name, Arg}]}}.
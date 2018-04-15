%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2018 10:47 PM
%%%-------------------------------------------------------------------
-module(etcp_acceptor).
-author("pouriya").

-export([start_link/5
        ,mode/1
        ,sleep/1
        ,accept/1]).

-export([init/6
        ,loop/3
        ,handle_debug/3]).

%% API
-export([]).
-include("internal/etcp_guard.hrl").
-include("internal/etcp_acceptor.hrl").
-define(DEF_DEBUG, []).
-define(DEF_TRANSPORT_MODULE, 'etcp_transporter_tcp').
-define(DEF_START_OPTIONS, []).
-define(GEN_CALL_TAG, '$gen_call').
-define(S, state).
-record(?S, {module
            ,init_argument
            ,name
            ,options
            ,mode
            ,process_registry
            ,listen_socket
            ,transporter_options
            ,transporter}).

-spec
start_link(module(), term(), etcp_types:start_options(), etcp_types:socket(), pid()) ->
    etcp_types:start_return().
start_link(Mod, InitArg, Opts, LSock, ProcReg) when erlang:is_atom(Mod) andalso
                                                    erlang:is_map(Opts) andalso
                                                    ?is_process_registry(ProcReg) ->
    proc_lib:start_link(?MODULE, init, [erlang:self(), Mod, InitArg, Opts, LSock, ProcReg]).


mode(Acceptor) when erlang:is_pid(Acceptor) ->
    gen_server:call(Acceptor, mode).


sleep(Acceptor) when erlang:is_pid(Acceptor) ->
    gen_server:call(Acceptor, ?ACCEPTOR_SLEEP_MODE).


accept(Acceptor) when erlang:is_pid(Acceptor) ->
    gen_server:call(Acceptor, ?ACCEPTOR_ACCEPT_MODE).


init(Parent, Mod, InitArg, Opts, LSock, ProcReg) ->
    DbgOpts = etcp_utils:get_value(acceptor_debug, Opts, ?DEF_DEBUG, fun erlang:is_list/1),
    TrMod = etcp_utils:get_value(transporter, Opts, ?DEF_TRANSPORT_MODULE, fun erlang:is_atom/1),
    Mode = etcp_utils:get_value(acceptor_mode, Opts, ?DEF_ACCEPTOR_MODE, fun filter_mode/1),
    TrOpts = etcp_utils:get_value(acceptor_mode, Opts, ?DEF_ACCEPTOR_MODE, fun(_) -> true end),
    proc_lib:init_ack(Parent, {ok, erlang:self()}),
    Name = erlang:self(),
    Dbg = etcp_utils:debug_options(?MODULE, Name, DbgOpts),
    Dbg2 = debug(Name, Dbg, {start, TrMod, LSock, ProcReg, Mode}),
    State = #?S{module = Mod
               ,init_argument = InitArg
               ,name = Name
               ,options = Opts
               ,mode = Mode
               ,process_registry = ProcReg
               ,transporter_options = TrOpts
               ,transporter = TrMod
               ,listen_socket = LSock},
    loop(Parent, Dbg2, State).


loop(Parent
    ,Dbg
    ,#?S{mode = ?ACCEPTOR_ACCEPT_MODE
        ,transporter = TrMod
        ,listen_socket = LSock
        ,transporter_options = TrOpts
        ,options = Opts
        ,name = Name
        ,module = Mod
        ,init_argument = InitArg
        ,process_registry = ProcReg}=State) ->
    case etcp_transporter:accept(TrMod, LSock, TrOpts) of
        {ok, Sock} ->
            Dbg2 = debug(Name, Dbg, {accept, Sock}),
            {ok, ConPid} = etcp_connection:start_link(Mod, InitArg, Opts, Sock),
            _ = etcp_transporter:controlling_process(TrMod, Sock, ConPid, TrOpts),
            Dbg3 = debug(Name, Dbg2, {start_connection, Sock, ConPid}),
            Dbg4 =
                if
                    erlang:is_pid(ProcReg) ->
                        etcp_server_process_registry:new(ProcReg, ConPid),
                        debug(Name, Dbg3, {process_registry, Sock, ConPid, ProcReg});
                    true ->
                        Dbg3
                end,
            process_message(Parent, Dbg4, State);
        {error, {socket_accept, [{reason, timeout}|_]}} ->
            process_message(Parent, Dbg, State);
        {error, {socket_accept, [{reason, emfile}|_]}} ->
            error_logger:warning_msg("ETCP acceptor ~p: too many alive connections", [Name]),
            process_message(Parent, Dbg, State);
        {error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end;
loop(Parent, Dbg, State) ->
    process_message(Parent, Dbg, State).


process_message(Parent, Dbg, #?S{name = Name}=State) ->
    receive
        Msg ->
            process_message(Parent, debug(Name, Dbg, {in, Msg}), State, Msg)
    after 0 ->
        ?MODULE:loop(Parent, Dbg, State)
    end.


process_message(Parent, Dbg, #?S{name = Name}=State, {?GEN_CALL_TAG, From, Request}) ->
    {Dbg2, State2} = process_request(debug(Name, Dbg, {call, From, Request}), State, From, Request),
    ?MODULE:loop(Parent, Dbg2, State2).


process_request(Dbg, #?S{name = Name, mode = Mode}=State, From, mode) ->
    {reply(Name, Dbg, From, Mode), State};
process_request(Dbg, #?S{name = Name}=State, From, Mode) when Mode =:= ?ACCEPTOR_ACCEPT_MODE orelse
                                                              Mode =:= ?ACCEPTOR_SLEEP_MODE ->
    {reply(Name, Dbg, From, Mode), State#?S{mode = Mode}};
process_request(Dbg, #?S{name = Name}=State, From, Request) ->
    {reply(Name, Dbg, From, {error, {unknown, [{request, Request}]}}), State}.


terminate(_, _, Rsn) ->
    erlang:exit(Rsn).


debug(_, [], _) ->
    [];
debug(Name, Dbg, Event) ->
    sys:handle_debug(Dbg, fun ?MODULE:handle_debug/3, Name, Event).


handle_debug(IODev, Event, Name) ->
    io:format(IODev, "*DBG* ETCP acceptor ~p got debug event ~p\n", [Name, Event]).


filter_mode(?ACCEPTOR_ACCEPT_MODE) ->
    true;
filter_mode(?ACCEPTOR_SLEEP_MODE) ->
    true;
filter_mode(_) ->
    false.


reply(Name, Dbg, {Pid, Tag}=Client, Msg) ->
    catch Pid ! {Tag, Msg},
    debug(Name, Dbg, {out, Client, Msg});
reply(Name, Dbg, Pid, Msg) ->
    catch Pid ! Msg,
    debug(Name, Dbg, {out, Pid, Msg}).
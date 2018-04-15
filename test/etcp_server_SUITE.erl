-module(etcp_server_SUITE).
-export([init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ,all/0
        ,'1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ]).%,'6'/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("etcp.hrl").

-define(CALLBACK, etcp_server_callback).
-define(HOST, "127.0.0.1").
-define(HOST_TUPLE, {127, 0, 0, 1}).
-define(TCP_SERVER_OPTS, [{reuseaddr, true}]).
-define(START_OPTIONS, #{connection_debug => [trace], acceptor_debug => [trace]}).
-define(print(Txt, Args), begin timer:sleep(10), ct:pal(Txt, Args) end).


init_per_suite(Config) ->
    application:start(sasl),
    Config.



end_per_suite(_Config) ->
    application:stop(sasl),
    ok.



init_per_testcase(_TCName, Config) ->
    erlang:process_flag(trap_exit, true),
    Config.



end_per_testcase(_TCName, _Config) ->
    ok.





all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
        || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports)) - 8)].



'1'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    Self = erlang:self(),
    F =
        fun(Opts, _) when Opts =:= ?START_OPTIONS ->
            F0 =
                fun(#etcp_metadata{transporter = etcp_transporter_tcp, transporter_options = [], socket = Sock}) when Sock =/= undefined ->
                    {ok, [{state, Self}]}
                end,
            {ok, F0}
        end,
    ?print("Starting server.", []),
    {ok, S} = etcp:start_link_server(?CALLBACK, F, Port, ?START_OPTIONS),
    ?print("Info: ~p~n", [[erlang:element(2, director:get_childspec(S, Id)) || {Id, _} <- erlang:element(2, director:get_pids(S))]]),

    {ok, AccRootSupPid} = director:get_pid(S, etcp_acceptor_root_sup),
    ?print("Acceport Supervisors: ~p~n", [director:which_children(AccRootSupPid)]),
    Pids = [erlang:element(2, El) || El <- director:which_children(AccRootSupPid)],
    ?print("Accseptors: ~p~n", [[director:which_children(Pid) || Pid <- Pids]]),
    AccSups = lists:concat([[director:get_childspec(AccSup, AccId)||{AccId, _} <- erlang:element(2, director:get_pids(AccSup))] || {_, AccSup} <- erlang:element(2, director:get_pids(AccRootSupPid))]),
    ?print("Acceptor Info: ~p~n", [AccSups]),

    ?print("Fetching server connections.", []),
    ?assertEqual([], etcp:fetch_server_connections(S)),

    ?print("Stopping server.", []),
    ?assertEqual(ok, etcp:stop_server(S)),
    ?print("Server stopped.", []),
    ok.


'2'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    Self = erlang:self(),
    F =
        fun(_, _) ->
            F0 =
                fun(_) ->
                    {ok, [{state, Self}]}
                end,
            {ok, F0}
        end,
    ?print("Starting server.", []),
    {ok, S} = etcp:start_link_server(?CALLBACK, F, Port, ?START_OPTIONS),
    Pkt = [65 || _ <- lists:seq(1, 100)], % "A..."
    ConnectAndSendPktFun =
        fun() ->
            ?print("Connecting...", []),
            {ok, C} = gen_tcp:connect(?HOST, Port, []),
            ?print("Connected, sending packet...", []),
            ok = gen_tcp:send(C, Pkt),
            ?print("packet has been sent, wait for response...", []),
            receive
                {tcp, C, Pkt0} when Pkt0 =:= Pkt ->
                    ?print("got response, closing connection...", []),
                    ok = gen_tcp:close(C),
                    ?print("connection closed", []),
                    erlang:exit(normal)
            end
        end,
    ConCount = 10,
    ?print("Starting ~p connections", [ConCount]),
    ok = lists:foreach(fun(_) -> erlang:spawn_link(ConnectAndSendPktFun) end, lists:seq(1, ConCount)),
    timer:sleep(3000),
    ServerConPids = etcp:fetch_server_connections(S),
    ?assertEqual(erlang:length(ServerConPids), ConCount),
    [etcp_test_utils:run_callback(ConPid
                                 ,?CALLBACK
                                 ,handle_packet
                                 ,fun(Pkt0, _, _) when Pkt0 =:= Pkt ->
            {ok, [{packet, Pkt}]} end) || ConPid <- ServerConPids],
    [etcp_test_utils:run_callback(ConPid
                                 ,?CALLBACK
                                 ,handle_disconnect
                                 ,fun(_, _) ->
            ok end) || ConPid <- ServerConPids],
    [etcp_test_utils:run_callback(ConPid
                                 ,?CALLBACK
                                 ,terminate
                                 ,fun(_, _, _) ->
            ok end) || ConPid <- ServerConPids],
    timer:sleep(3000),
    ?assertEqual([], etcp:fetch_server_connections(S)),
    ?assertEqual(ok, etcp:stop_server(S)),
    ok.


'3'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    Self = erlang:self(),
    F =
        fun(_, _) ->
            F0 =
                fun(_) ->
                    {ok, [{state, Self}]}
                end,
            {ok, F0}
        end,
    ?print("Starting server.", []),
    {ok, S} = etcp:start_link_server(?CALLBACK, F, Port, (?START_OPTIONS)#{acceptor_count => 2}),

    ?print("Fetching server connections.", []),
    ?assertEqual([], etcp:fetch_server_connections(S)),

    ?print("Fetching server accpeotors.", []),
    Accs = etcp:fetch_acceptors(S),
    ?assertMatch([{_, _}, {_, _}], Accs),

    ?print("Fetching server acceptor modes.", []),
    ?assertEqual(accept, etcp:modes(S)),

    ?print("Waking up acceptors.", []),
    ?assertEqual(ok, etcp:accept(S)),

    ?print("Fetching server acceptor modes.", []),
    ?assertEqual(accept, etcp:modes(S)),

    ?print("Turning accpeotrs to sleep modes.", []),
    ?assertEqual(ok, etcp:sleep(S)),

    ?print("Fetching server acceptor modes.", []),
    ?assertEqual(sleep, etcp:modes(S)),

    ?print("Stoping server.", []),
    ?assertEqual(ok, etcp:stop_server(S)),
    ok.


'4'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    Self = erlang:self(),
    F =
        fun(_, _) ->
            F0 =
                fun(_) ->
                    {ok, [{state, Self}]}
                end,
            {ok, F0}
        end,
    ?print("Starting server.", []),
    {ok, S} = etcp:start_link_server(?CALLBACK, F, Port, #{acceptor_count => 1, connection_process_regisrty => false}),
    ?assertEqual(no_process_registry, etcp:fetch_server_connections(S)),
    ?assertEqual(ok, etcp:stop_server(S)),
    ok.


'5'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, []),
    Self = erlang:self(),
    F =
        fun(_, _) ->
            F0 =
                fun(_) ->
                    {ok, [{state, Self}]}
                end,
            {ok, F0}
        end,
    ?print("Starting server.", []),
    ?assertMatch({error, {socket_listen, [{reason, _}|_]}}, etcp:start_link_server(?CALLBACK, F, Port, ?START_OPTIONS)),
    gen_tcp:close(LSock),
    ok.


%%'6'(_Cfg) ->
%%    Port = etcp_test_utils:choose_port(),
%%    ?print("Port number: ~p", [Port]),
%%    Self = erlang:self(),
%%    F =
%%        fun(_, _) ->
%%            F0 =
%%                fun(_) ->
%%                    {ok, [{state, Self}]}
%%                end,
%%            {ok, F0}
%%        end,
%%    ?print("Starting server.", []),
%%    {ok, _S} = etcp:start_link_server(?CALLBACK, F, Port, #{transporter_options => [{backlog, 1000}]}),
%%    Connector =
%%        fun(Count) ->
%%            Connect =
%%                fun(_) ->
%%                    T1 = etcp_test_utils:timestamp(),
%%                    {ok, _} = gen_tcp:connect(?HOST, Port, []),
%%                    self() ! {connecting_time, etcp_test_utils:timestamp() - T1},
%%                    ok
%%                end,
%%            lists:foreach(Connect, lists:seq(1, Count)),
%%            Recv =
%%                fun(_, Acc) ->
%%                    receive
%%                        {connecting_time, T} ->
%%                            [T|Acc]
%%                    end
%%                end,
%%            Ts = lists:foldl(Recv, [], lists:seq(1, Count)),
%%            Self ! {average, lists:sum(Ts)/erlang:length(Ts)}
%%        end,
%%    Recv =
%%        fun(_, Acc) ->
%%            receive
%%                {average, T} ->
%%                    [T|Acc]
%%            end
%%        end,
%%    Count = 100,
%%    Count2 = 100,
%%    lists:foreach(fun(_) -> erlang:spawn(fun() -> Connector(Count2) end) end, lists:seq(1, Count)),
%%    Ts = lists:foldl(Recv, [], lists:seq(1, Count)),
%%    timer:sleep(5000),
%%    ?print("Threads: ~p - Con per thread: ~p - Connecting time average: ~p~n", [Count, Count2, (lists:sum(Ts)/erlang:length(Ts)) / 1000000]).
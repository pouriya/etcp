-module(etcp_connection_SUITE).
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
        ,'6'/1
        ,'7'/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("etcp.hrl").

-define(CALLBACK, etcp_connection_callback).
-define(HOST, "127.0.0.1").
-define(HOST_TUPLE, {127, 0, 0, 1}).
-define(TCP_SERVER_OPTS, [{reuseaddr, true}]).
-define(START_OPTS, #{connection_debug => [trace]}).
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
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_SERVER_OPTS),
    Self = erlang:self(),
    F =
        fun(_) ->
            {ok, [{state, Self}]}
        end,
    {ok, Pid} = etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS),
    {ok, Sock} = gen_tcp:accept(LSock),
    ?print("Connected to ~p:~p", [?HOST, Port]),

    Pkt1 = "hello ",
    Timeout = 100,
    gen_tcp:send(Sock, Pkt1),
    ?print("Packet ~p has been sent from server to client\n"
           "Wating for receiving packet from client", [Pkt1]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_packet
                                ,fun(Pkt, _, #etcp_metadata{}) when Pkt =:= Pkt1 -> {ok, [{timeout, Timeout}]} end),
    ?print("Packet ~p received from client\n"
           "Wating for timeout '~p'"
          ,[Pkt1, Timeout]),
    timer:sleep(Timeout),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_info
                                ,fun(timeout, _, #etcp_metadata{}) -> {ok, [{timeout, infinity}]} end),
    ?print("Client process got timeout after '~p'", [Timeout]),

    Pkt2 = "world!",
    gen_tcp:send(Sock, Pkt2),
    ?print("Packet ~p has been sent from server to client\n"
           "Wating for receiving packet from client", [Pkt2]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_packet
                                ,fun(Pkt, _, #etcp_metadata{}) when Pkt =:= Pkt2 -> {ok, [{packet, " "}]} end),
    receive
        {tcp, _, " "} ->
            ok
    end,
    ?print("Packet ~p received from client", [Pkt2]),

    Info = info,
    Pid ! Info,
    ?print("Message ~p has been sent to client\n"
           "Wating for receiving message from client", [Info]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_info
                                ,fun(Msg, _, #etcp_metadata{}) when Msg =:= Info -> ok end),
    ?print("Message ~p received from client", [Info]),

    Cast = cast,
    gen_server:cast(Pid, Cast),
    ?print("Generic cast message ~p has been sent to client\n"
           "Wating for receiving message from client", [Cast]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_cast
                                ,fun(Msg, _, #etcp_metadata{}) when Msg =:= Cast -> ok end),
    ?print("Generic cast message ~p received from client", [Cast]),

    Req = request,
    Res = response,
    CallFun =
        fun() ->
            Self ! {erlang:self(), gen_server:call(Pid, Req)},
            erlang:exit(normal)
        end,
    Caller = erlang:spawn_link(CallFun),
    ?print("Generic call '~p' has been sent to client\n"
           "Wating for receiving message '~p' from client", [Req, Res]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_call
                                ,fun(Msg, From, _, #etcp_metadata{}) when Msg =:= Req -> {ok, [{reply, {From, Res}}]} end),
    ?print("Generic call '~p' received from client and reply message '~p' sent to caller\n"
           "Waiting for receiving caller response"
          ,[Req, Res]),
    receive
        {Caller, Res} ->
            ok
    end,
    ?print("Caller receives response '~p' from client process", [Res]),

    ok = gen_tcp:close(Sock),
    ?print("Connection closed by server\n"
           "Waiting for client reaction", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_disconnect
                                ,fun(_, #etcp_metadata{}) -> ok end),
    ?print("Client knew disconnection\n"
           "Waiting for termination", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,terminate
                                ,fun(normal, _, #etcp_metadata{}) -> ok end),
    timer:sleep(10),
    false = erlang:is_process_alive(Pid),
    ?print("Client terminates", []),
    gen_tcp:close(LSock).


'2'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_SERVER_OPTS),
    Self = erlang:self(),
    F =
        fun(#etcp_metadata{})->
            {ok, [{state, Self}, {timeout, 0}]}
        end,
    {ok, Pid} = etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS),
    {ok, _Sock} = gen_tcp:accept(LSock),
    ?print("Connected to ~p:~p", [?HOST, Port]),

    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_info
                                ,fun(timeout, _, #etcp_metadata{}) -> {ok, [{timeout, infinity}]} end),
    gen_tcp:close(LSock).


'3'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_SERVER_OPTS),
    SockOpts = [{mode, binary}, {active, false}],
    Self = erlang:self(),
    F =
        fun(#etcp_metadata{}) ->
            {ok, [{state, Self}]} end,
    {ok, Pid} = etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, etcp_test_utils:merge([?START_OPTS, #{transporter_options => SockOpts}])),
    {ok, Sock} = gen_tcp:accept(LSock),
    Pkt = <<"foo">>,
    ok = gen_tcp:send(Sock, Pkt),
    ?print("Packet '~p' has been send from server to client\n"
           "Wating for receiving packet from client"
          ,[Pkt]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_packet
                                ,fun(Pkt0, _, #etcp_metadata{}) when Pkt0 =:= Pkt -> ok end),
    ?print("Packet '~p' received from client", [Pkt]),
    ok = gen_tcp:close(Sock),
    ?print("Connection closed by server\n"
           "Waiting for client reaction", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_disconnect
                                ,fun(_, #etcp_metadata{}) -> ok end),
    ?print("Client knew disconnection\n"
           "Waiting for termination", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,terminate
                                ,fun(normal, _, #etcp_metadata{}) -> ok end),
    timer:sleep(10),
    false = erlang:is_process_alive(Pid),
    ?print("Client terminates", []),
    gen_tcp:close(LSock).


'4'(Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}|?TCP_SERVER_OPTS]),
    Self = erlang:self(),
    F =
        fun(#etcp_metadata{}) ->
            {ok, [{state, Self}]}
        end,
    {ok, Pid} = etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS),
    ServSSLHandshakeFun =
        fun() ->
            {ok, Sock} = gen_tcp:accept(LSock),
            KeyFile = etcp_test_utils:get_ssl_key_file(Cfg),
            CertFile = etcp_test_utils:get_ssl_cert_file(Cfg),
            {ok, SSLSock} = ssl:ssl_accept(Sock, [{keyfile, KeyFile}, {certfile, CertFile}], 1000),
            ok = ssl:controlling_process(SSLSock, Self),
            Self ! {erlang:self(), SSLSock}
        end,
    ServSSLHandshaker = spawn_link(ServSSLHandshakeFun),
    ?print("Server SSL acceptor process created", []),
    Info = info,
    Pid ! Info,
    ?print("Waiting for client SSL handshake", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_info
                                ,fun(Info0, _, #etcp_metadata{socket = CSock}) when Info0 =:= Info ->
                                     {ok, CSSLSock} = ssl:connect(CSock, []),
                                     {ok, [{transporter, etcp_transporter_ssl}, {socket, CSSLSock}]}
                                 end),
    ?print("Client mades SSL handshake", []),
    SSLSock =
        receive
            {ServSSLHandshaker, SSLSock0} ->
                SSLSock0
        end,

    Pkt = "bar",
    ok = ssl:send(SSLSock, Pkt),
    ?print("Packet '~p' has been sent from server to client\n"
           "Wating for receiving '~p' packet from client", [Pkt, Pkt]),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_packet
                                ,fun(Pkt0, _, #etcp_metadata{}) when Pkt0 =:= Pkt  -> ok end),
    ?print("Packet '~p' received from client", [Pkt]),
    ok = ssl:close(SSLSock),
    ?print("Connection closed by server\n"
           "Waiting for client reaction", []),
    timer:sleep(100),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_disconnect
                                ,fun(_, #etcp_metadata{}) -> ok end),
    ?print("Client knew disconnection\n"
           "Waiting for termination", []),
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,terminate
                                ,fun(normal, _, #etcp_metadata{}) -> ok end),
    timer:sleep(10),
    false = erlang:is_process_alive(Pid),
    ?print("Client terminates", []),
    gen_tcp:close(LSock).


'5'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, [{backlog, 10}, {active, false}|?TCP_SERVER_OPTS]),
    F =
        fun(_) ->
            {stop, foo, []}
        end,
    ?assertEqual({error, foo}, etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS)),

    F2 =
        fun(_) ->
            bar
        end,
    ?assertMatch({error, {return, [{value, bar}|_]}}, etcp:start_link_connection(?CALLBACK, F2, ?HOST, Port, ?START_OPTS)),

    F3 =
        fun(_) ->
            {ok, [{packet, erlang:self()}]}
        end,
    ?assertMatch({error, {socket_send, [{reason, _}|_]}}, etcp:start_link_connection(?CALLBACK, F3, ?HOST, Port, ?START_OPTS)),

    F4 =
        fun(_) ->
            ignore
        end,
    ?assertMatch(ignore, etcp:start_link_connection(?CALLBACK, F4, ?HOST, Port, ?START_OPTS)),

    F5 =
        fun(_) ->
            close
        end,
    ?assertMatch({error, normal}, etcp:start_link_connection(?CALLBACK, F5, ?HOST, Port, ?START_OPTS)),

    F6 =
        fun(_) ->
            {ok, [{timeout, 3.14}]}
        end,
    ?assertMatch({error, {option_value, [{timeout, _}|_]}}, etcp:start_link_connection(?CALLBACK, F6, ?HOST, Port, ?START_OPTS)),

    F7 =
        fun(_) ->
            {ok, [{transporter, "oops"}]}
        end,
    ?assertMatch({error, {option_value, [{transporter, _}|_]}}, etcp:start_link_connection(?CALLBACK, F7, ?HOST, Port, ?START_OPTS)),

    F8 =
        fun(_) ->
            {ok, [{reply, who}]}
        end,
    ?assertMatch({error, {option_value, [{reply, _}|_]}}, etcp:start_link_connection(?CALLBACK, F8, ?HOST, Port, ?START_OPTS)),

    F9 =
        fun(_) ->
            {ok, [{hibernate, not_a_bool}]}
        end,
    ?assertMatch({error, {option_value, [{hibernate, _}|_]}}, etcp:start_link_connection(?CALLBACK, F9, ?HOST, Port, ?START_OPTS)),

    gen_tcp:close(LSock),
    ok.


'6'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}|?TCP_SERVER_OPTS]),
    ok = gen_tcp:close(LSock),
    F =
        fun(_) ->
            {stop, foo, []}
        end,
    ?assertMatch({error, {socket_connect, [{reason, _}|_]}}, etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS)),
    ok.


'7'(_Cfg) ->
    Port = etcp_test_utils:choose_port(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}|?TCP_SERVER_OPTS]),
    Self = erlang:self(),
    F =
        fun(_) ->
            {ok, [{state, Self}, {hibernate, true}]}
        end,
    {ok, Pid} = etcp:start_link_connection(?CALLBACK, F, ?HOST, Port, ?START_OPTS),
    ?assertEqual({current_function, {erlang, hibernate, 3}}, erlang:process_info(Pid, current_function)),

    Pid ! wakeup,
    etcp_test_utils:run_callback(Pid
                                ,?CALLBACK
                                ,handle_info
                                ,fun(wakeup, _, _) -> ok end),
    ?assertNotEqual({current_function, {erlang, hibernate, 3}}, erlang:process_info(Pid, current_function)),
    erlang:exit(Pid, kill),
    gen_tcp:close(LSock),
    ok.
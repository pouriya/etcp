-module(etcp_transporter_).

-export(['1'/2]).

-define(print(Txt, Args), begin timer:sleep(10), ct:pal(Txt, Args) end).
-define(HOST, {127, 0, 0, 1}).

-include_lib("eunit/include/eunit.hrl").

'1'(Callback, Opts) ->
    Self = erlang:self(),

    ?print("Callback module: ~p\n"
           "Options: ~p"
          ,[Callback, Opts]),
    Port = etcp_test_utils:choose_port(),

    ?print("Opening listening socket", []),
    {ok, LSock} = Callback:listen(Port, Opts),
    ?print("Listening socket created, Checking socket activity", []),
    ?assertEqual(false, get_options(Callback, LSock, Opts, active)),

    ?print("Creating acceptor process", []),
    AccFun =
        fun() ->
            ?print("Accepting ... ", []),
            {ok, Sock} = Callback:accept(LSock, Opts),
            ?print("Accepted new connection, calling controlling_process", []),
            ok = Callback:controlling_process(Sock, Self, Opts),
            ?print("Sending socket to controller process", []),
            Self ! {erlang:self(), Sock},
            ?print("Stop accepting", []),
            erlang:exit(normal)
        end,
    Acc = erlang:spawn_link(AccFun),

    ?print("Connecting to ~p:~p", [?HOST, Port]),
    {ok, CSock} = Callback:connect(?HOST, Port, Opts, 3000),
    ?print("Connected to ~p:~p, Checking socket activity", [?HOST, Port]),
    ?assertEqual(true, get_options(Callback, CSock, Opts, active)),

    ?print("Waiting for acceptor", []),
    SSock =
        receive
            {Acc, SSock0} ->
                ?print("Client socket received from acceptor, Checking socket activity", []),
                ?assertEqual(false, get_options(Callback, SSock0, Opts, active)),
                receive
                    {'EXIT', Acc, normal} ->
                        ?print("Acceptor process exited normally", []),
                        ok
                end,
                SSock0
        end,

    ?print("Make server socket active", []),
    ?assertEqual(ok, Callback:activate(SSock, Opts)),
    ?assertEqual(true, get_options(Callback, SSock, Opts, active)),

    Pkt1 = "foo",
    ?print("Sending packet '~p' through server socket", [Pkt1]),
    ?assertEqual(ok, Callback:send(SSock, Pkt1, Opts)),
    ?print("Packet '~p' has been sent from server socket", [Pkt1]),

    ?print("Waiting for receiving '~p' packet from client side", [Pkt1]),
    Pkt1 =
        receive
            Msg1 ->
                ?print("Erlang message '~p' received, Checking message using callback module ~p"
                      ,[Msg1, Callback]),
                Result = Callback:check_message(Msg1, CSock, Opts),
                ?assertMatch({ok, _}, Result),
                erlang:element(2, Result)
        end,
    ?print("Packet '~p' received from client", [Pkt1]),

    Pkt2 = "bar",
    ?print("Sending packet '~p' through client socket", [Pkt2]),
    ?assertEqual(ok, Callback:send(CSock, Pkt2, Opts)),
    ?print("Packet '~p' has been sent from client socket", [Pkt2]),

    ?print("Waiting for receiving '~p' packet from server side", [Pkt2]),
    Pkt2 =
        receive
            Msg2 ->
                ?print("Erlang message '~p' received", [Msg2]),
                {ok, Pkt2_0} = Callback:check_message(Msg2, SSock, Opts),
                Pkt2_0
        end,
    ?print("Packet '~p' received from server", [Pkt2]),

    ?print("Making server socket inactive", []),
    ?assertEqual(ok, Callback:set_options(SSock, [{active, false}], Opts)),
    ?print("Checking server socket activity", []),
    ?assertEqual(false, get_options(Callback, SSock, Opts, active)),

    ?print("Making client socket inactive", []),
    ?assertEqual(ok, Callback:set_options(CSock, [{active, false}], Opts)),
    ?print("Checking client socket activity", []),
    ?assertEqual(false, get_options(Callback, CSock, Opts, active)),

    Pkt3 = "baz",
    ?print("Sending packet '~p' through client socket", [Pkt3]),
    Callback:send(CSock, Pkt3, Opts),
    ?print("Packet '~p' has been sent from client socket", [Pkt3]),

    ?print("Waiting for receiving '~p' packet from server side", [Pkt3]),
    Pkt3 =
        begin
            Pkt3Head = erlang:hd(Pkt3),
            {ok, [Pkt3Head]} = Callback:recv(SSock, 1, infinity, Opts),
            {ok, Pkt3Tail} = Callback:recv(SSock, 0, infinity, Opts),
            [Pkt3Head|Pkt3Tail]
        end,
    ?print("Packet '~p' received from server", [Pkt3]),

    Pkt4 = "qux",
    ?print("Sending packet '~p' through server socket", [Pkt4]),
    Callback:send(SSock, Pkt4, Opts),

    ?print("Packet '~p' has been sent from server socket", [Pkt4]),

    ?print("Waiting for receiving '~p' packet from client side", [Pkt4]),
    Pkt4 =
        begin
            Pkt4Head = erlang:hd(Pkt4),
            {ok, [Pkt4Head]} = Callback:recv(CSock, 1, infinity, Opts),
            {ok, Pkt4Tail} = Callback:recv(CSock, 0, infinity, Opts),
            [Pkt4Head|Pkt4Tail]
        end,
    ?print("Packet '~p' received from client", [Pkt4]),

    ?print("Change client socket to non-blocking mode", []),
    ?assertEqual(ok, Callback:set_options(CSock, [{active, true}], Opts)),

    ?print("Closing connection from server", []),
    ?assertEqual(ok, Callback:close(SSock, Opts)),

    ?print("Waiting for receiving event about disconnection from client", []),
    receive
        Msg3 ->
            ?assertEqual({error, [{reason, closed}]}, Callback:check_message(Msg3, CSock, Opts))
    end,
    ?print("Client connection closed too", []).


get_options(Mod, Sock, Opts) ->
    Result = Mod:get_options(Sock, Opts),
    ?assertMatch({ok, _}, Result),
    {_, Result2} = Result,
    ?assert(erlang:is_list(Result2)),
    Result2.


get_options(Mod, Sock, Opts, Key) ->
    Result = lists:keyfind(Key, 1, get_options(Mod, Sock, Opts)),
    ?assertMatch({Key, _}, Result),
    erlang:element(2, Result).
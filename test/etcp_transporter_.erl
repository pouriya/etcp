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
    {ok, S} = Callback:init(Opts),
    {ok, {LSock, SS}} = Callback:listen(Port, S),
    ?print("Listening socket created", []),

    ?print("Creating acceptor process", []),
    AccFun =
        fun() ->
            ?print("Accepting ... ", []),
            {ok, {Sock, SS2}} = Callback:accept(LSock, SS),
            ?print("Accepted new connection, calling controlling_process", []),
            {ok, SS3} = Callback:controlling_process(Sock, Self, SS2),
            ?print("Sending socket to controller process", []),
            Self ! {erlang:self(), Sock, SS3},
            ?print("Stop accepting", []),
            erlang:exit(normal)
        end,
    Acc = erlang:spawn_link(AccFun),

    ?print("Connecting to ~p:~p", [?HOST, Port]),
    {ok, {CSock, CS}} = Callback:connect(?HOST, Port, S),
    ?print("Connected to ~p:~p", [?HOST, Port]),

    ?print("Waiting for acceptor", []),
    {SSock, SS3} =
        receive
            {Acc, SSock0, SS2} ->
                ?print("Client socket received from acceptor", []),
                receive
                    {'EXIT', Acc, normal} ->
                        ?print("Acceptor process exited normally", []),
                        ok
                end,
                {SSock0, SS2}
        end,

    Pkt1 = "foo",
    ?print("Sending packet '~p' through server socket", [Pkt1]),
    {ok, SS4} = Callback:send(SSock, Pkt1, SS3),
    ?print("Packet '~p' has been sent from server socket", [Pkt1]),

    ?print("Waiting for receiving '~p' packet from client side", [Pkt1]),
    {Pkt1, CS3} =
        receive
            Msg1 ->
                ?print("Erlang message '~p' received, Checking message using callback module ~p"
                      ,[Msg1, Callback]),
                {ok, {Pkt2, CS2}} = Callback:check_message(Msg1, CSock, CS),
                {Pkt2, CS2}
        end,
    ?print("Packet '~p' received from client", [Pkt1]),

    Pkt3 = "bar",
    ?print("Sending packet '~p' through client socket", [Pkt2]),
    {ok, CS4} =  Callback:send(CSock, Pkt3, CS3),
    ?print("Packet '~p' has been sent from client socket", [Pkt2]),

    ?print("Waiting for receiving '~p' packet from server side", [Pkt2]),
    {Pkt3, SS6} =
        receive
            Msg2 ->
                ?print("Erlang message '~p' received", [Msg2]),
                {ok, {Pkt4, SS5}} = Callback:check_message(Msg2, SSock, SS4),
                {Pkt4, SS5}
        end,
    ?print("Packet '~p' received from server", [Pkt3]),

    ?print("Making server socket inactive", []),
    {ok, SS7} =  Callback:set_options(SSock, [{active, false}], SS6),
    ?print("Checking server socket activity", []),
    {false, SS8} =  get_options(Callback, SSock, SS7, active),

    ?print("Making client socket inactive", []),
    {ok, CS5} = Callback:set_options(CSock, [{active, false}], CS4),
    ?print("Checking client socket activity", []),
    {false, CS6} = get_options(Callback, CSock, CS5, active),

    Pkt5 = "baz",
    ?print("Sending packet '~p' through client socket", [Pkt5]),
    {ok, CS7} = Callback:send(CSock, Pkt5, CS6),
    ?print("Packet '~p' has been sent from client socket", [Pkt5]),

    ?print("Waiting for receiving '~p' packet from server side", [Pkt3]),
    {Pkt5, SS9} =
        begin
            Pkt5Head = erlang:hd(Pkt5),
            {ok, {[Pkt5Head], SS10}} = Callback:recv(SSock, 1, infinity, SS8),
            {ok, {Pkt5Tail, SS11}} = Callback:recv(SSock, 0, infinity, SS10),
            {[Pkt5Head|Pkt5Tail], SS11}
        end,
    ?print("Packet '~p' received from server", [Pkt5]),

    Pkt7 = "qux",
    ?print("Sending packet '~p' through server socket", [Pkt7]),
    {ok, CS8} = Callback:send(SSock, Pkt7, CS7),
    ?print("Packet '~p' has been sent from server socket", [Pkt7]),

    ?print("Waiting for receiving '~p' packet from client side", [Pkt7]),
    {Pkt7, CS9} =
        begin
            Pkt7Head = erlang:hd(Pkt7),
            {ok, {[Pkt7Head], CS10}} = Callback:recv(CSock, 1, infinity, CS8),
            {ok, {Pkt7Tail, CS11}} = Callback:recv(CSock, 0, infinity, CS10),
            {[Pkt7Head|Pkt7Tail], CS11}
        end,
    ?print("Packet '~p' received from client", [Pkt7]),

    ?print("Change client socket to non-blocking mode", []),
    {ok, CS12} = Callback:set_options(CSock, [{active, true}], CS9),

    ?print("Closing connection from server", []),
    {ok, _} = Callback:close(SSock, SS9),

    ?print("Waiting for receiving event about disconnection from client", []),
    receive
        Msg3 ->
            ?assertEqual({error, [{reason, closed}]}, Callback:check_message(Msg3, CSock, CS12))
    end,
    ?print("Client connection closed too", []).


get_options(Mod, Sock, S1) ->
    Result = Mod:get_options(Sock, S1),
    ?assertMatch({ok, {_, _}}, Result),
    {_, {Result2, S2}} = Result,
    ?assert(erlang:is_list(Result2)),
    {Result2, S2}.


get_options(Mod, Sock, S, Key) ->
    {Result2, S2} = get_options(Mod, Sock, S),
    Result = lists:keyfind(Key, 1, Result2),
    ?assertMatch({Key, _}, Result),
    {erlang:element(2, Result), S2}.
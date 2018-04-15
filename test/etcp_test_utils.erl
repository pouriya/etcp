-module(etcp_test_utils).
-export([merge/1
        ,run_callback/4
        ,choose_port/0
        ,get_ssl_cert_file/1
        ,get_ssl_key_file/1
        ,make_callback_return/2
        ,timestamp/0
        ,run_log/3]).



merge(Maps) ->
    F =
        fun(Map, Acc) ->
            maps:merge(Map, Acc)
        end,
    lists:foldl(F, #{}, Maps).


run_callback(Pid, CallbackMod, CallbackFunc, Fun) ->
    receive
        {Pid, Ref, CallbackMod, CallbackFunc} ->
            Pid ! {Ref, Fun}
    after 1000 ->
        exit({timeout, [{pid, Pid}
                       ,{callback_module, CallbackMod}
                       ,{callback_function, CallbackFunc}
                       ,{'fun', Fun}
                       ,{timeout, 1000}
                       ,{messages, lists:reverse(messages())}]})
    end.


make_callback_return(Pid, {Mod, Func, Args}) ->
    run_log(Mod, Func, Args),
    Ref = erlang:make_ref(),
    Pid ! {erlang:self(), Ref, Mod, Func},
    receive
        {Ref, Fun} ->
            erlang:apply(Fun, Args)
    end.


messages() ->
    receive
        Msg ->
            [Msg|messages()]
    after 0 ->
        []
    end.


choose_port() ->
    Ports = lists:seq(7070, 8080),
    Port = lists:nth(rand:uniform(erlang:length(Ports)), Ports),
    ct:pal("Port number: ~p", [Port]),
    Port.


get_ssl_key_file(Cfg) ->
    filename:join(proplists:get_value(data_dir, Cfg), "key.pem").


get_ssl_cert_file(Cfg) ->
    filename:join(proplists:get_value(data_dir, Cfg), "cert.pem").


timestamp() ->
    {Mega, S, Micro} = os:timestamp(),
    ((Mega * 1000000) + S) * 1000000 + Micro.


run_log(Mod, Func, Args) ->
    ct:pal("RUN STATE: ~s:~s/~p with arguments \n~p", [Mod, Func, erlang:length(Args), Args]).
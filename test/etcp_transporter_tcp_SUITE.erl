-module(etcp_transporter_tcp_SUITE).
-export([init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ,all/0
        ,'1'/1]).

-define(TESTER_MOD, etcp_transporter_).
-define(CALLBACK_MOD, etcp_transporter_tcp).


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
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports)) - 7)].





'1'(_Cfg) ->
    ?TESTER_MOD:'1'(?CALLBACK_MOD, []).
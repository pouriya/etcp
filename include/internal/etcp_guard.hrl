-define(is_proc_ref(Arg), (erlang:is_pid(Arg) orelse
                           erlang:is_atom(Arg) orelse
                           erlang:is_tuple(Arg))).

-define(is_timeout(Arg), ((erlang:is_integer(Arg) andalso Arg > -1) orelse Arg =:= infinity)).

-define(is_table_type(Arg), ((Arg =:= list) orelse
                             (erlang:is_tuple(Arg) andalso
                              (erlang:element(1, Arg) =:= ets orelse
                              erlang:element(1, Arg) =:= mnesia) andalso
                             erlang:is_atom(erlang:element(2, Arg))))).

-define(is_host(Arg), ((erlang:is_list(Arg)) orelse
                       (erlang:is_tuple(Arg) andalso erlang:tuple_size(Arg) == 4))). % {127,0,0,1}

-define(is_process_registry(Arg), (erlang:is_pid(Arg) orelse Arg =:= undefined)).
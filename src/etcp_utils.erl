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
%% @hidden
%% -------------------------------------------------------------------------------------------------
-module(etcp_utils).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([get_value/2
        ,get_value/3
        ,get_value/4
        ,is_timeout/1
        ,is_whole_integer/1
        ,debug_options/3
        ,filter_socket_options/1
        ,stacktrace/1
        ,filter_logger/1
        ,filter_logger2/1
        ,filter_plan/1
        ,concat/1
        ,filter_table_type/1
        ,make_proper_list/1]).

%% -------------------------------------------------------------------------------------------------
%% API:

get_value(Key, Opts) ->
    do_get_value(Key, Opts, undefined, undefined).


get_value(Key, Opts, Def) ->
    do_get_value(Key, Opts, {default, Def}, undefined).


%% @doc
%%      gets value of Key from Opts. if not found, default value Def
%%      will returned. founded value will pass to filter fun Filter/1.
%% @end
get_value(Key, Opts, Def, Filter) ->
    do_get_value(Key, Opts, {default, Def}, Filter).


-spec
stacktrace(pos_integer()) ->
    list().
stacktrace(Count) ->
    case erlang:process_info(erlang:self(), [current_stacktrace]) of
        [{current_stacktrace, StackTrace}] ->
            if
                erlang:length(StackTrace) > Count ->
                    lists:nthtail(Count, StackTrace);
                true ->
                    StackTrace
            end;
        undefined ->
            []
    end.


-spec
is_timeout(term()) ->
    boolean().
is_timeout(infinity) ->
    true;
is_timeout(Timeout) when erlang:is_integer(Timeout) ->
    if
        Timeout >= 0 ->
            true;
        true ->
            false
    end.


-spec
is_whole_integer(term()) ->
    boolean().
is_whole_integer(Int) when erlang:is_integer(Int) ->
    if
        Int >= 0 ->
            true;
        true ->
            false
    end.


-spec
debug_options(term(), pid(), [] | [sys:dbg_opt()]) ->
    list().
debug_options(Name, Pid, DbgOpts) ->
    try
        sys:debug_options(DbgOpts)
    catch
        _ErrorType:_Reason ->
            error_logger:warning_report("~p [~p]: bad debug options: ~p", [Pid, Name, DbgOpts]),
            []
    end.


-spec
filter_socket_options(list()) ->
    'ok' | etcp_types:error().
filter_socket_options(SockOpts) when erlang:is_list(SockOpts) ->
    case lists:keytake(active, 1, SockOpts) of
        {value, {_, Val}, SockOpts2} ->
            if
                erlang:is_boolean(Val) ->
                    filter_socket_options(SockOpts2);
                true ->
                    {error, {socket_options, [{active, Val}, {socket_options, SockOpts}]}}
            end;
        false ->
            ok
    end;
filter_socket_options(Other) ->
    {error, {socket_options_type, [{socket_options, Other}]}}.

filter_logger(Logger) when erlang:is_function(Logger, 2) ->
    true;
filter_logger(_) ->
    false.


filter_logger2(Logger) when erlang:is_function(Logger, 1) ->
    true;
filter_logger2(_) ->
    false.


filter_plan(Plan) when erlang:is_function(Plan, 1) ->
    true;
filter_plan(_) ->
    false.


concat(List) ->
    concat(List, []).


filter_table_type(list) ->
    {ok, []};
filter_table_type({Type, Name}) when (Type =:= ets orelse Type =:= mnesia) andalso
                                     erlang:is_atom(Name) ->
    Mod =
        case Type of
            ets ->
                director_table_ets;
            _ ->
                director_table_mnesia
        end,
    {ok, [{table_module, Mod}, {table_init_argument, Name}]};
filter_table_type(_) ->
    false.


make_proper_list([]) ->
    [];
make_proper_list(L) ->
    do_make_proper_list(L, []).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

do_get_value(Key, Opts, Def, Filter) ->
    try maps:get(Key, Opts) of
        Val ->
            case Filter of
                undefined ->
                    Val;
                Filter ->
                    try Filter(Val) of
                        ok ->
                            Val;
                        {ok, Val2} ->
                            Val2;
                        true ->
                            Val;
                        false ->
                            erlang:error({value_type, [{key, Key}
                                                             ,{value, Val}
                                                             ,{stacktrace, stacktrace(2)}]});
                        {error, Reason} ->
                            erlang:error({bad_value, [{reason, Reason}
                                                     ,{key, Key}
                                                     ,{value, Val}
                                                     ,{stacktrace, stacktrace(2)}]})
                    catch
                        _:Rsn ->
                            erlang:error({filter_value, [{reason, Rsn}
                                                        ,{key, Key}
                                                        ,{value, Val}
                                                        ,{stacktrace, stacktrace(2)}]})
                    end
            end
    catch
        _:_ ->
            case Def of
                {default, DefVal} ->
                    DefVal;
                _ ->
                    erlang:error({value_not_found, [{key, Key}
                                                   ,{stacktrace, stacktrace(2)}]})
            end
    end.


concat([[Elem|List3]|List], List2) ->
    concat([List3|List], [Elem|List2]);
concat([[]|List], List2) ->
    concat(List, List2);
concat([], List2) ->
    List2.


do_make_proper_list([El|Rest], Ret) when erlang:is_list(Rest) ->
    do_make_proper_list(Rest, [El|Ret]);
do_make_proper_list([], Ret) ->
    lists:reverse(Ret);
do_make_proper_list([El|Rest], Ret) ->
    lists:reverse([Rest, El | Ret]).
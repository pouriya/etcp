%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 12:57 AM
%%%-------------------------------------------------------------------
-module(etcp_server_process_registry).
-author("pouriya").
-export([start_link/0
        ,new/2
        ,fetch/1]).

-export([init/1
        ,handle_call/3
        ,handle_info/2
        ,terminate/2]).

-define(NEW_CONNECTION_TAG, new).

%% API
-export([]).


start_link() ->
    gen_server:start_link(?MODULE, undefined, []).


new(ProcReg, ConPid) ->
    ProcReg ! {?NEW_CONNECTION_TAG, ConPid},
    ok.


fetch(ProcReg) ->
    gen_server:call(ProcReg, fetch).

init(_) ->
    {ok, []}.


handle_info({?NEW_CONNECTION_TAG, ConPid}, State) ->
    _ = erlang:monitor(process, ConPid),
    {noreply, [ConPid|State]};
handle_info({'DOWN', _, _, ConPid, _}, State) ->
    {noreply, lists:delete(ConPid, State)}.

handle_call(fetch, _, State) ->
    {reply, State, State}.


terminate(_, _) ->
    ok.
-module(etcp_connection_callback).
-export([connection_init/2
        ,handle_packet/3
        ,handle_call/4
        ,handle_cast/3
        ,handle_info/3
        ,handle_disconnect/2
        ,code_change/3
        ,terminate/3]).


connection_init(InitArg, MD) ->
    etcp_test_utils:run_log(?MODULE, connection_init, [InitArg, MD]),
    InitArg(MD).


handle_packet(Packet, State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, handle_packet, [Packet, State, SMD]}).


handle_call(Call, From, State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, handle_call, [Call, From, State, SMD]}).


handle_cast(Cast, State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, handle_cast, [Cast, State, SMD]}).


handle_info(Msg, State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, handle_info, [Msg, State, SMD]}).


terminate(Reason, State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, terminate, [Reason, State, SMD]}).


handle_disconnect(State, SMD) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, handle_disconnect, [State, SMD]}).


code_change(Old, State, Extra) ->
    etcp_test_utils:make_callback_return(State, {?MODULE, code_change, [Old, State, Extra]}).
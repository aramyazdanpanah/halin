-module(halin_ws_conn).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("halin.hrl").
-include("api.hrl").

init(Req, _Opts) ->
    {IP, Port} = maps:get(peer, Req),
    ?LOG_INFO("one clinet connected , IP => ~p, Port => ~p", [IP, Port]),
    State = #conn_state{client_ip = IP,
                        client_port = Port},
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	NewState = State#conn_state{conn_pid = self()},
	{ok, NewState}.

websocket_handle({text, Msg}, #conn_state{user_id = UserID} = State) ->
    ?LOG_DEBUG("**** << Incoming Frame, user_id: ~p, Frame = ~p",[UserID, Msg]),
    case handle_data(Msg, State) of
        {reply, Reply, NewState} ->
            ReReply = halin_api_transformer:reform(Reply),
            BinReply = halin_codec:encode(ReReply),
            ?LOG_DEBUG("****>> Output Frame, user_id: ~p, Frame = ~p",[UserID, BinReply]),
            {reply, {text, BinReply}, NewState};

        {noreply, NewState} ->
            {noreply, NewState};

        {drop, NewState} ->
            {stop, NewState}
    end;
websocket_handle(Data, State) ->
    ?LOG_ERROR("reciecved unknown data => ~p", [Data]),
	{ok, State}.

websocket_info({signal, Signal}, #conn_state{user_id = UserID} = State) ->
    ReSignal = halin_api_transformer:reform(Signal),
    BinSignal = halin_codec:encode(ReSignal),
    ?LOG_DEBUG("****>> Output Signal, user_id: ~p, Frame = ~p",[UserID, BinSignal]),
	{reply, {text, Signal}, State};
websocket_info(_Info, State) ->
	{ok, State}.

terminate(Reason, PartialReq, State) ->
    ?LOG_INFO("one conn terminate, Reason => ~p ~n, PartialReq => ~p ~n, State => ~p ~n", [Reason, PartialReq, State]),
    ok.
%% === internal functions
-spec handle_data(binary(), conn_state()) -> {reply, reply(), conn_state()} |
                                             {noreply, conn_state()} |
                                             {drop, conn_state()}.
handle_data(Data, State) when is_binary(Data) ->
        case halin_codec:decode(Data) of
            {ok, JsonMap} ->
                handle_data(JsonMap, State);

            {error, malformed_data} ->
                {reply, generate_main_error(malformed_data), State}
        end;
handle_data(JsonMap, State) when is_map(JsonMap) ->
    Request = halin_api_transformer:transform(JsonMap),
    try
        halin_ws_conn_handler:handle(Request, State)
    catch A:B ->
            print_stacktrace(),
            ?LOG_ERROR("-- Exeption :~p, ~p ", [A, B]),
            {reply, generate_main_error(internal_server_error), State}
    end.

generate_main_error(malformed_data) ->
    #reply{api_version = null,
           request_id = null,
           verb = 'Error',
           params = #'Error'{error_type = <<"MalformedData">>,
                             description = <<"malform json.">>}};
generate_main_error(internal_server_error) ->
    #reply{api_version = null,
           request_id = null,
           verb = 'Error',
           params = #'Error'{error_type = <<"InternalServerError">>,
                             description = <<"internal server error.">>}}.

print_stacktrace() ->
    ?LOG_ERROR("Stacktrace => ~p ~n", [erlang:get_stacktrace()]).

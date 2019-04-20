-module(halin_ws_conn).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("halin.hrl").

init(Req, _Opts) ->
    {IP, Port} = maps:get(peer, Req),
    ?LOG_INFO("one clinet connected , IP => ~p, Port => ~p", [IP, Port]),
    State = #conn_state{client_ip = IP,
                        client_port = Port},
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	NewState = State#conn_state{conn_pid = self()},
	{ok, NewState}.

websocket_handle({text, Msg}, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.

terminate(Reason, PartialReq, State) ->
    ?LOG_INFO("one conn terminate, Reason => ~p ~n, PartialReq => ~p ~n, State => ~p ~n", [Reason, PartialReq, State]),
    ok.

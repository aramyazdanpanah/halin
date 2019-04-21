-module(halin_ws_conn_handler).

-export([handle/2]).

-include("halin.hrl").
-include("api.hrl").

-spec handle(request(), conn_state()) -> {reply, reply(), conn_state()} |
                                         {noreply, conn_state()} |
                                         {drop, conn_state}.
%%%%% =====================================================================
%% DoAuthentication
%%%%% =====================================================================
handle(#request{api_version = ApiVersion,
                request_id = RequestID,
                sess_key = _ConnPID,
                verb = 'DoAuthentication',
                params = #'DoAuthentication'{user_id = UserID,
                                             token = Token}},
       #conn_state{} = State) when
      UserID =/= undefined,
      Token =/= undefined ->
    Reply = #reply{api_version = ApiVersion,
                   request_id = RequestID,
                   verb = 'AuthParams',
                   params = #'AuthParams'{sess_key = self()}},
    NewState = State#conn_state{auth = true,
                                user_id = UserID},
    {reply, Reply, NewState};

handle(#request{api_version = ApiVersion,
                request_id = RequestID,
                sess_key = _ConnPID,
                verb = 'DoAuthentication',
                params = #'DoAuthentication'{user_id = _UserID,
                                            token = _Token}},
       #conn_state{} = State) ->
    Reply = #reply{api_version = ApiVersion,
                   request_id = RequestID,
                   verb = 'Error',
                   params = #'Error'{error_type = <<"InvalidParams">>,
                                     description = <<"please fill all fields.">>}},
    {reply, Reply, State};

%%%%% =====================================================================
%% GetActiveSessions
%%%%% =====================================================================
handle(#request{api_version = ApiVersion,
                request_id = RequestID,
                sess_key = ConnPID,
                verb = 'GetActiveSessions',
                params = #'GetActiveSessions'{}},
       #conn_state{conn_pid = ConnPID, auth = true} = State) ->

    Reply = #reply{api_version = ApiVersion,
                   request_id = RequestID,
                   verb = 'ActiveSessions',
                   params = #'ActiveSessions'{sessions = []}},
    {reply, Reply, State};

handle(#request{api_version = ApiVersion,
                request_id = RequestID,
                sess_key = PID,
                verb = 'DoAuthentication',
                params = #'DoAuthentication'{user_id = _UserID,
                                            token = _Token}},
       #conn_state{conn_pid = ConnPID} = State) ->
    Reply = #reply{api_version = ApiVersion,
                   request_id = RequestID,
                   verb = 'Error',
                   params = #'Error'{error_type = <<"InvalidParams">>,
                                     description = <<"please fill all fields.">>}},
    {reply, Reply, State};

handle(_, _) ->
    ok.

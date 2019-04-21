%% -*- mode:erlang -*-

-ifndef(HEADER_API).
-define(HEADER_API, true).

-record(request, {api_version :: binary(),
                  request_id :: integer(),
                  sess_key :: pid(),
                  verb :: atom(),
                  params :: request_params()}).

-record(reply, {api_version :: binary(),
                request_id :: integer(),
                verb :: atom(),
                params :: reply_params()}).

-record(signal, {api_version :: binary(),
                 verb :: atom(),
                 params :: reply_params()}).

-type reply() :: #reply{}.
-type request() :: #request{}.
-type signal() :: #signal{}.
%% === request DoAuthentication
-record('DoAuthentication', {user_id :: integer(),
                             token :: string()}).

-record('AuthParams', {sess_key :: pid()}).

%% === request GetActiveSessions
-record(session, {sess_id :: integer(),
                  publish_url :: string(),
                  play_url :: [string()],
                  start_date :: string(),
                  start_time :: string(),
                  description :: string()}).

-record('GetActiveSessions', {}).

-record('ActiveSessions', {sessions :: [#session{}]}).


%% === request SubscribeOnSession
-record('SubscribeOnSession', {sess_id :: integer()}).

-record('Done', {}).

%% === request PublishMsg
-record(msg_info, {msg_type :: text | voice,
                   publisher_name :: string(),
                   timestamp :: integer()}).

-record(msg_body, {text_body :: string(),
                   voice_oid :: string(),
                   voice_duration :: integer()}).

-record('PublishMsg', {sess_id :: integer(),
                       msg_info :: #msg_info{},
                       msg_body :: #msg_body{}}).

-record('Msg', {sess_id :: integer(),
                msg_info :: #msg_info{},
                msg_body :: #msg_body{}}).

%% === request GetOnlineStats
-record('GetOnlineStats', {sess_id :: integer()}).

-record('OnlineStats', {sess_id :: integer(),
                        online_viewer :: integer(),
                        like :: integer(),
                        dislike :: integer()}).

%% === Error
-record('Error', {error_type :: string(),
                  description :: string()}).

%% === types
-type request_params() :: #'DoAuthentication'{} | y.
-type reply_params() :: #'AuthParams'{} | y.

-endif.

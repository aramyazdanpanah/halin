-module(halin_api_transformer).

-export([transform/1,
         reform/1]).


-include("halin.hrl").
-include("api.hrl").

transform(Request) when is_map(Request) ->
    ApiVersion = maps:get(<<"ApiVersion">>, Request),
    RequestID = maps:get(<<"RequestID">>, Request),
    SessKey = transform_sess_key(maps:get(<<"SessKey">>, Request, undefined)),
    Verb = transform_verb(maps:get(<<"Verb">>, Request, undefined)),
    Params = transform(Verb, maps:get(<<"Params">>, Request, undefined)),
    #request{api_version = ApiVersion,
             request_id = RequestID,
             sess_key = SessKey,
             verb = Verb,
             params = Params}.

transform_sess_key(undefined) ->
    undefined;
transform_sess_key(Else) ->
    List = erlang:binary_to_list(Else),
    erlang:list_to_pid(List).

transform_verb(<<"DoAuthentication">>) ->
    'DoAuthentication';
transform_verb(<<"GetActiveSessions">>) ->
    'GetActiveSessions';
transform_verb("SubscribeOnSession") ->
    'SubscribeOnSession';
transform_verb(<<"PublishMsg">>) ->
    'PublishMsg';
transform_verb(<<"GetOnlineStats">>) ->
    'GetOnlineStats'.

transform('DoAuthentication', Params) ->
    #'DoAuthentication'{user_id = maps:get(<<"UserID">>, Params),
                        token = maps:get(<<"Token">>, Params)};
transform('GetActiveSessions', _Params) ->
    #'GetActiveSessions'{};
transform('SubscribeOnSession', Params) ->
    #'SubscribeOnSession'{sess_id = maps:get(<<"SessionID">>, Params)};
transform('PublishMsg', Params) ->
    MsgInfoInMap = maps:get(<<"MsgInfo">>, Params),
    MsgBodyInMap = maps:get(<<"MsgBody">>, Params),
    MsgType = transform_msg_type(maps:get(<<"MsgType">>, MsgInfoInMap)),
    MsgInfo = #msg_info{msg_type = MsgType},
    MsgBody = case MsgType of
                  text ->
                      #msg_body{text_body = maps:get(<<"TextBody">>, MsgBodyInMap)};

                  voice ->
                      #msg_body{voice_oid = maps:get(<<"VoiceOID">>, MsgBodyInMap),
                                voice_duration = maps:get(<<"VoiceDuration">>, MsgBodyInMap)}
              end,
    #'PublishMsg'{sess_id = maps:get(<<"SessionID">>, Params),
                  msg_info = MsgInfo,
                  msg_body = MsgBody};
transform('GetOnlineStats', Params) ->
    #'GetOnlineStats'{sess_id = maps:get(<<"SessionID">>, Params)}.

transform_msg_type(<<"TEXT">>) ->
    text;
transform_msg_type(<<"VOICE">>) ->
    voice.


reform(Request) ->
     ok.

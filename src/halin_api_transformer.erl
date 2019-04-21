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


reform(#reply{api_version = ApiVersion,
              request_id = RequestID,
              verb = Verb,
              params = Params}) ->
    #{<<"ApiVersion">> => ApiVersion,
      <<"RequestID">> => RequestID,
      <<"Verb">> => reform_verb(Verb),
      <<"Params">> => reform(Verb, Params)};
reform(#signal{api_version = ApiVersion,
               verb = Verb,
               params = Params}) ->
    #{<<"ApiVersion">> => ApiVersion,
      <<"RequestID">> => null,
      <<"Verb">> => reform_verb(Verb),
      <<"Params">> => reform(Verb, Params)}.

reform_verb('AuthParams') ->
    <<"AuthParams">>;
reform_verb('ActiveSessions') ->
    <<"ActiveSessions">>;
reform_verb('Done') ->
    <<"Done">>;
reform_verb('Msg') ->
    <<"Msg">>;
reform_verb('OnlineStats') ->
    <<"OnlineStats">>;
reform_verb('Error') ->
    <<"Error">>.

reform('AuthParams', #'AuthParams'{sess_key = PID}) ->
    #{<<"SessKey">> => erlang:list_to_binary(erlang:pid_to_list(PID))};
reform('ActiveSessions', #'ActiveSessions'{sessions = Sessions}) ->
    ReSessions = [#{<<"SessionID">> => SessionID,
                    <<"PublishUrl">> => PublishUrl,
                    <<"PlayUrl">> => PlayListUrl,
                    <<"StartDate">> => StartDate,
                    <<"StartTime">> => StartTime,
                    <<"Description">> => Description} || #session{sess_id = SessionID,
                                                                  publish_url = PublishUrl,
                                                                  play_url = PlayListUrl,
                                                                  start_date = StartDate,
                                                                  start_time = StartTime,
                                                                  description = Description}  <- Sessions],
    #{<<"Sessions">> => ReSessions};
reform('Done', #'Done'{}) ->
    #{};
reform('Msg', #'Msg'{sess_id = SessionID,
                     msg_info = #msg_info{msg_type = MsgType} = MsgInfo,
                     msg_body = MsgBody}) ->
    #{<<"SessionID">> => SessionID,
      <<"MsgInfo">> => reform_msg_info(MsgInfo),
      <<"MsgBody">> => reform_msg_body(MsgType, MsgBody)};
reform('OnlineStats', #'OnlineStats'{sess_id = SessionID,
                                     online_viewer = OnlineViewer,
                                     like = Like,
                                     dislike = Dislike}) ->
    #{<<"SessionID">> => SessionID,
      <<"OnlineViewer">> => OnlineViewer,
      <<"Like">> => Like,
      <<"Dislike">> => Dislike};
reform('Error', #'Error'{error_type = ErrorType,
                         description = Description}) ->
    #{<<"ErrorType">> => ErrorType,
      <<"Discription">> => Description}.


reform_msg_type(text) ->
    <<"TEXT">>;
reform_msg_type(voice) ->
    <<"VOICE">>.

reform_msg_info(#msg_info{msg_type = MsgType,
                          publisher_name = PublisherName,
                          timestamp = Timestamp}) ->
    #{<<"MsgType">> => reform_msg_type(MsgType),
      <<"PublisherName">> => PublisherName,
      <<"Timestamp">> => Timestamp}.

reform_msg_body(text, #msg_body{text_body = TextBody}) ->
    #{<<"TextBody">> => TextBody};
reform_msg_body(voice, #msg_body{voice_oid = OID,
                                 voice_duration = Duration}) ->
    #{<<"VoiceOID">> => OID,
      <<"VoiceDuration">> => Duration}.

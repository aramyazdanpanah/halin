%% -*- mode:erlang -*-

-ifndef(HEADER_HALIN).
-define(HEADER_HALIN, true).

-record(conn_state, {client_ip :: tuple(),
                     client_port :: non_neg_integer(),
                     conn_pid :: pid(),
                     user_id :: non_neg_integer(),
                     auth :: true | false}).

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.

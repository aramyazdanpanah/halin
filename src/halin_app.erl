%%%-------------------------------------------------------------------
%% @doc halin public API
%% @end
%%%-------------------------------------------------------------------

-module(halin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("halin.hrl").
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    PrivDir = code:priv_dir(halin),
    ?LOG_INFO("====> ~p", [PrivDir]),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", cowboy_static, {file, code:priv_dir(halin) ++ "/index.html"}},
                                             {"/streamauth", halin_stream_auth, []},
                                             {"/api", halin_ws_conn, []},
                                             {"/static/[...]", cowboy_static, {file, code:priv_dir(halin) ++ "/static"}}
                                            ]}
                                     ]),
	{ok, _} = cowboy:start_clear(http, [{port, 8090}], #{
                                         env => #{dispatch => Dispatch}
                                        }),
    halin_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

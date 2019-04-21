-module(halin_codec).

-export([decode/1,
         encode/1]).

-define(CODEC, jiffy).

-spec decode(binary()) -> {ok, map()} | {error, malformed_data}.
decode(Msg) ->
    try
        {ok, ?CODEC:decode(Msg, [return_maps])}
    catch _:_ ->
            {error, malformed_data}
    end.

-spec encode(map()) -> {ok, binary()} | {error, malformed_map}.
encode(Msg) ->
    try
        {ok, ?CODEC:encode(Msg)}
    catch _:_ ->
            {error, malformed_map}
    end.

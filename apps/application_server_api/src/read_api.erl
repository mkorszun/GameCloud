-module(read_api).

-export([out/1]).

out(_Args) ->
    {html, "<h1>Game save read API</h1>"}.

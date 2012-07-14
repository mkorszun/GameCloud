-module(delete_api).

-export([out/1]).

out(_Args) ->
    {html, "<h1>Game save delete API</h1>"}.

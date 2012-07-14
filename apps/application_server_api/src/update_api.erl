-module(update_api).

-export([out/1]).

out(_Args) ->
    {html, "<h1>Game save update API<h1>"}.

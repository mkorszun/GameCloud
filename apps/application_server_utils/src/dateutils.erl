-module(dateutils).

-export([timestamp/0, timestamp/1]).

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

timestamp(Shift) ->
    timestamp() + 60 * Shift.
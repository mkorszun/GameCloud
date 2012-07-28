-module(parameter).
-export([delete/2]).

delete([], Proplist) -> Proplist;
delete([K | T], Proplist) ->
    delete(T, proplists:delete(K, Proplist)).

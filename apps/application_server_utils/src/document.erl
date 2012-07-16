-module(document).

-export([create/1]).

create([]) -> 
    [];
create([{K,V}|T])  ->
    [{bin(K), bin(V)} | create(T)].
 
bin(Val) when is_binary(Val) ->
    Val;
bin(Val) when is_list(Val) ->
    list_to_binary(Val).

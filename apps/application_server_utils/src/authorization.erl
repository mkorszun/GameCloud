%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([auth/2]).

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> true;
auth(_, _) -> false.
    

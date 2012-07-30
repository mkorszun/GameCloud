%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([authorize/2, authorize/3]).

authorize(DB, Params) ->
    User = proplists:get_value("user_id", Params),
    Pass = proplists:get_value("password", Params),
    authorize(DB, User, Pass).

authorize(DB, User, Pass) ->
    case database:read_doc(DB, User) of
        {ok, UserDoc} ->
            UserPass = document:read("password", UserDoc),
            {ok, auth(UserPass, Pass)};
        {error, Reason} ->
            {error, Reason}
    end.

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> true;
auth(_, _) -> false.
    

-module(read_api).
-export([out/1]).

-define(DB, couchdb_db).
-define(APP, application_server_api).

out(A) ->
    Args = yaws_api:parse("user_id", A),
    User = proplists:get_value("user_id", Args),
    Pass = proplists:get_value("password", Args),
    Game = proplists:get_value("game", Args),
    Name = proplists:get_value("name", Args),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    read_game(DB, User, Pass, Game, Name).

read_game(_, undefined, _, _, _) ->
    [{status, 404}, {content, "text/xml", "Missing user name"}];
read_game(_, _, undefined, _, _) ->
    [{status, 404}, {content, "text/xml", "Missing password"}];
read_game(_, _, _, undefined, _) ->
    [{status, 400}, {content, "text/xml", "Missing game name"}];
read_game(_, _, _, _, undefined) ->
    [{status, 400}, {content, "text/xml", "Missing save name"}];
read_game(_, _, _, _, _) ->
    [{status, 200}, {content, "text/xml", "ok"}].

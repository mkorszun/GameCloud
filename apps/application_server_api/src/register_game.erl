-module(register_game).
-export([out/1]).

-define(DB, couchdb_db).
-define(APP, application_server_api). 

out(A) ->
    Args = yaws_api:parse_post(A),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    RegisterGame = fun() -> register_game(DB, Args) end,
    request:execute(validate(), Args, RegisterGame).

register_game(DB, Args) ->
    Game = proplists:get_value("game_name", Args),
    Desc = proplists:get_value("description", Args),
    Doc = document:create([{"_id", Game}, {"description", Desc}]),
    case database:save_doc(DB, Doc) of
        {ok, _} ->
            [{status, 200}, {content, "text/xml", "ok"}];
        {error, conflict} ->
            [{status, 409}, {content, "text/xml", "Game already exists"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

validate() ->
    [
        {"game_name", undefined, 400, "text/xml", "Missing game name"},
        {"game_name", [], 400, "text/xml", "Empty game name"},
        {"description", undefined, 400, "text/xml", "Missing description"},
        {"description", [], 400, "text/xml", "Empty description"}
    ].

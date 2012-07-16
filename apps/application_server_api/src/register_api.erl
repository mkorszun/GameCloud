-module(register_api).

-export([out/1]).

out(A) ->
    Args = yaws_api:parse_post(A),
    User = proplists:get_value("user_id", Args),
    Pass = proplists:get_value("password", Args),
    Conn = couchbeam:server_connection(),
    {ok, DB} = couchbeam:open_db(Conn, "save_cloud"),
    Doc = document:create([{"_id", User}, {"pass", Pass}]),
    case couchbeam:save_doc(DB, {Doc}) of
	{ok, _} ->
	    [{status, 200}, {content, "text/xml", "ok"}]; 
	{error, conflict} ->
	    [{status, 409}, {content, "text/xml", "User already exists"}];
	{error, _Error} ->
	    [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

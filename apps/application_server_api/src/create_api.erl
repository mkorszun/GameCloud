%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Save creation API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(create_api).
-export([out/1]).

-define(DB, couchdb_db).
-define(APP, application_server_api).

%% ###############################################################
%% CALLBACK FUNCTION
%% ############################################################### 

out(A) ->
    case yaws_api:parse_multipart_post(A) of
	    {cont, Cont, Res} ->
	        St = multipart:handle_res(A, Res, [], []),
	        {get_more, Cont, [St | multipart:state(A)]};
        {result, Res} ->
            {P, F} = multipart:handle_res(A, Res, [], []),
	        {ok, DBName} = application:get_env(?APP, ?DB),
	        {ok, DB} = database:open(DBName),
            Create = fun() -> create_save(DB, P, F) end,
	        request:execute(validate(), P, Create);
        {error, _Reason} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

create_save(DB, Params, Files) ->
    User = proplists:get_value("user_id", Params),
    case database:read_doc(DB, User) of
	    {ok, UserDoc} ->
	        UserPass = document:read("password", UserDoc),
            Pass = proplists:get_value("password", Params), 
	        AuthRes = authorization:auth(UserPass, Pass),
            Doc = parameter:delete(["password"], Params),
	        create_save(DB, document:create(Doc), Files, AuthRes);
	    {error, not_found} ->
	        [{status, 404},{content, "text/xml", "User not found"}];
	    {error, _Error} ->
	        [{status, 500}, {content, "text/xml", "Internal error"}]
    end.
     
create_save(DB, Doc, Files, true) ->
    {ok, Doc1} = database:save_doc(DB, Doc, Files),
    DocId = document:get_id(Doc1),
    [{status, 200}, {content, "text/xml", DocId}];
create_save(_, _, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

validate() ->
    [
        {"user_id", undefined, 404, "text/xml", "Missing user name"},
        {"user_id", [], 404, "text/xml", "Empty user name"},
        {"password", undefined, 400, "text/xml", "Missing password"},
        {"password", [], 400, "text/xml", "Empty password"},
        {"game", undefined, 400, "text/xml", "Missing game name"},
        {"game", [], 400, "text/xml", "Empty game name"},
        {"name", undefined, 400, "text/xml", "Missing save name"},
        {"name", [], 400, "text/xml", "Empty save name"},
        {"type", undefined, 400, "text/xml", "Missing type"},
        {"type", [], 400, "text/xml", "Empty type"}
    ].
    
%% ###############################################################
%% ###############################################################
%% ############################################################### 

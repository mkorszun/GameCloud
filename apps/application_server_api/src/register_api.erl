%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% User registration API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_api).
-export([out/1]).

-define(DB, couchdb_db).
-define(APP, application_server_api).

%% ###############################################################
%% CALLBACK FUNCTION
%% ############################################################### 

out(A) ->
    Args = yaws_api:parse_post(A),
    User = proplists:get_value("user_id", Args),
    Pass = proplists:get_value("password", Args),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open( DBName),
    register_user(DB, User, Pass).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

register_user(_, undefined, _) ->
    [{status, 404}, {content, "text/xml", "Missing user name"}];
register_user(_, _, undefined) ->
    [{status, 404}, {content, "text/xml", "Missing password"}];
register_user(_, [], _) ->
    [{status, 400}, {content, "text/xml", "Empty user name"}];
register_user(_, _, []) ->
    [{status, 400}, {content, "text/xml", "Empty password"}];

register_user(DB, User, Pass) ->
    Doc = document:create([{"_id", User}, {"password", Pass}]),
    case database:save_doc(DB, Doc) of
	{ok, _} ->
	    [{status, 200}, {content, "text/xml", "ok"}]; 
	{error, conflict} ->
	    [{status, 409}, {content, "text/xml", "User already exists"}];
	{error, _Error} ->
	    [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

%% ###############################################################
%% ###############################################################
%% ############################################################### 

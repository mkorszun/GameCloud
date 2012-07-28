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
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open( DBName),
    Register = fun() -> register_user(DB, Args) end,
    request:execute(validate(), Args, Register).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

register_user(DB, Args) ->
    User = proplists:get_value("user_id", Args),
    Pass = proplists:get_value("password", Args), 
    Doc = document:create([{"_id", User}, {"password", Pass}]),
    case database:save_doc(DB, Doc) of
	    {ok, _} ->
	        [{status, 200}, {content, "text/xml", "ok"}]; 
	    {error, conflict} ->
	        [{status, 409}, {content, "text/xml", "User already exists"}];
	    {error, _Error} ->
	        [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

validate() ->
    [
        {"user_id", undefined, 404, "text/xml", "Missing user name"},
        {"user_id", [], 400, "text/xml", "Empty user name"},
        {"password", undefined, 404, "text/xml", "Missing password"},
        {"password", [], 400, "text/xml", "Empty password"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

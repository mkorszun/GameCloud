%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Delete player API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(delete_player_api).
-export([out/1]).

%% ###############################################################
%% INCLUDES
%% ###############################################################

-include("api.hrl").

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_post(A),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    Delete = fun() -> delete_user(DB, Args) end,
    request:execute(validate(), Args, Delete).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

delete_user(DB, Args) ->
    case authorization:authorize(player, DB, Args) of
        {ok, Result} ->
            delete_user(DB, Args, Result);
        {error, not_found} -> 
            [{status, 404},{content, "text/xml", "User not found"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.      

delete_user(DB, Args, true) ->
    User = proplists:get_value("user_id", Args),
    case database:delete_doc(DB, User) of
        {ok, _} ->
            [{status, 200}, {content, "text/xml", "ok"}];
        {error, empty} ->  
            [{status, 400}, {content, "text/xml", "Save not found"}];
        {error, _Reason} ->    
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end;    

delete_user(_, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

%% ############################################################### 
%% VALIDATE PARAMS
%% ############################################################### 

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

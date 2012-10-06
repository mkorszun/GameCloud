%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Delete save API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(delete_save_api).
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
    Delete = fun() -> save:delete(DB, Args) end,
    case request:execute(validate(), Args, Delete) of
        {ok, _} ->
            [{status, 200}, {content, "application/json", response:to_json("ok")}];
        {error, player_not_found} ->
            [{status, 400}, {content, "application/json", response:to_json("Player not found")}];
        {error, save_not_found} ->
            [{status, 400}, {content, "application/json", response:to_json("Save not found")}];
        {error, unauthorized} ->
            [{status, 404}, {content, "application/json", response:to_json("Unauthorized")}];
        {error, {missing_param, Code, Message}} ->
            [{status, Code}, {content, "appllication/json", response:to_json(Message)}];
        {error, _Error} ->
            [{status, 500}, {content, "application/json", response:to_json("Internal error")}]
    end.

%% ############################################################### 
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"player_uuid", undefined, 400, "Missing player uuid"},
        {"player_uuid", [], 400, "Empty player uuid"},
        {"password", undefined, 400, "Missing player password"},
        {"password", [], 400, "Empty player password"},
        {"save_uuid", undefined, 400, "Missing save uuid"},
        {"save_uuid", [], 400, "Empty save uuid"}
    ].

%% ############################################################### 
%% ############################################################### 
%% ############################################################### 

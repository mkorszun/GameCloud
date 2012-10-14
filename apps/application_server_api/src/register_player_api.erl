%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Register player API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_player_api).
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
    {ok, DB} = database:open( DBName),
    Register = fun() -> player:register(DB, Args) end,
    case request:execute(validate(), Args, Register) of
        {ok, Doc} ->
            [{status, 200}, {content, "application/json", response:to_json(document:get_id(Doc))}];
        {error, unauthorized} ->
            [{status, 404}, {content, "application/json", response:to_json("Unauthorized")}];
        {error, developer_not_found} ->
            [{status, 400}, {content, "application/json", response:to_json("Developer not found")}];
        {error, game_not_found} ->
            [{status, 400}, {content, "appllication/json", response:to_json("Game not found")}];
        {error, {missing_param, Code, Message}} ->
            [{status, Code}, {content, "appllication/json", response:to_json(Message)}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", response:to_json("Internal error")}]
    end.

%% ############################################################### 
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"developer_id", undefined, 400, "Missing developer id"},
        {"developer_id", [], 400, "Empty developer id"},
        {"dev_password", undefined, 400, "Missing developer password"},
        {"dev_password", [], 400, "Empty developer password"},
        {"game_uuid", undefined, 400, "Missing game uuid"},
        {"game_uuid", [], 400, "Empty game uuid"},
        {"player_id", undefined, 400, "Missing player id"},
        {"player_id", [], 400, "Empty player id"},
        {"password", undefined, 400, "Missing player password"},
        {"password", [], 400, "Empty player password"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

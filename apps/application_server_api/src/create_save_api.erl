%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Create save API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(create_save_api).
-export([out/1]).

%% ###############################################################
%% INCLUDES
%% ###############################################################

-include("api.hrl").

%% ###############################################################
%% CALLBACK FUNCTION
%% ############################################################### 

out(A) ->
    case yaws_api:parse_multipart_post(A) of
        {cont, Cont, Res} ->
            State = multipart:handle_res(A, Res, multipart:state(A)),
            {get_more, Cont, State};
        {result, Res} ->
            Params = multipart:handle_res(A, Res, multipart:state(A)),
            {P, F} = multipart:build_params(A, Params, [], []),
            {ok, DBName} = application:get_env(?APP, ?DB),
            {ok, DB} = database:open(DBName),
            Create = fun() -> save:register(DB, P, F) end,
            request(validate(), P, Create);
        {error, no_multipart_form_data} ->
            {ok, DBName} = application:get_env(?APP, ?DB),
            {ok, DB} = database:open(DBName),
            Args = yaws_api:parse_post(A),
            Create = fun() -> save:register(DB, Args, []) end,
            request(validate(), Args, Create);
        {error, _Reason} ->
            [{status, 500}, {content, "application/json", response:error("Internal error")}]
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

request(ValidationList, Args, CreateFun) ->
    case request:execute(ValidationList, Args, CreateFun) of
        {ok, Doc} ->
            [{status, 200}, {content, "application/json", response:ok(document:get_id(Doc))}];
        {error, game_not_found} ->
            [{status, 404}, {content, "application/json", response:error("Game not found")}];
        {error, player_not_found} ->
            [{status, 404}, {content, "application/json", response:error("Player not found")}];
        {error, unauthorized} ->
            [{status, 401}, {content, "application/json", response:error("Unauthorized")}];
        {error, {missing_param, Code, Message}} ->
            [{status, Code}, {content, "appllication/json", response:error(Message)}];
        {error, _Error} ->
            [{status, 500}, {content, "application/json", response:error("Internal error")}]
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
        {"game_uuid", undefined, 400, "Missing game uuid"},
        {"game_uuid", [], 400, "Empty game uuid"},
        {"save_name", undefined, 400, "Missing save name"},
        {"save_name", [], 400, "Empty save name"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

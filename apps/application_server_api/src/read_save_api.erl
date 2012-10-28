%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud 
%%% @doc
%%% Read save API
%%% @end 
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(read_save_api).
-export([out/1]).

%% ###############################################################
%% CALLBACK FUNCTION 
%% ############################################################### 

out(A) ->
    Args = yaws_api:parse_query(A),
    Fun = fun() -> save:read1(Args) end,
    request(request:get_method(A), validate(), Args, Fun).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

request('GET', ValidationList, Args, Fun) ->
    case request:execute(ValidationList, Args, Fun) of
        {ok, Save} ->
            [{status, 200}, {content, "application/json", response:ok(Save)}];
        {error, player_not_found} ->
            [{status, 404}, {content, "application/json", response:error("Player not found")}];
        {error, save_not_found} ->
            [{status, 404}, {content, "application/json", response:error("Save not found")}];
        {error, unauthorized} ->
            [{status, 401}, {content, "application/json", response:error("Unauthorized")}];
        {error, {missing_param, Code, Message}} ->
            [{status, Code}, {content, "appllication/json", response:error(Message)}];
        {error, _Error} ->
            [{status, 500}, {content, "application/json", response:error("Internal error")}]
    end;

request(_, _, _, _) ->
    [{status, 405}, {content, "application/json", response:error("Method not allowed")}].

%% ############################################################### 
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"player_uuid", undefined, 400, "Missing player uuid"},
        {"player_uuid", [], 400, "Empty player uuid"},
        {"password", undefined, 400, "Missing password"},
        {"password", [], 400, "Empty password"},
        {"save_uuid", undefined, 400, "Missing save uuid"},
        {"save_uuid", [], 400, "Empty save uuid"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

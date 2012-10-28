%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Delete game API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(delete_game_api).
-export([out/1]).

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_query(A),
    Fun = fun() -> game:delete1(Args) end,
    request(request:get_method(A), validate(), Args, Fun).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

request('DELETE', ValidationList, Args, Fun) ->
    case request:execute(ValidationList, Args, Fun) of
        {ok, Doc} ->
            [{status, 200}, {content, "application/json", response:ok("ok")}];
        {error, developer_not_found} ->
            [{status, 404}, {content, "appllication/json", response:error("Developer not found")}];
        {error, game_not_found} ->
            [{status, 404}, {content, "appllication/json", response:error("Game not found")}];
        {error, unauthorized} ->
            [{status, 401}, {content, "appllication/json", response:error("Unauthorized")}];
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
        {"developer_id", undefined, 400, "Missing developer id"},
        {"developer_id", [], 400, "Empty developer id"},
        {"password", undefined, 400, "Missing developer password"},
        {"password", [], 400, "Empty developer password"},
        {"game_uuid", undefined, 400, "Missing game uuid"},
        {"game_uuid", [], 400, "Empty game uuid"}
    ].

%% ###############################################################
%% ###############################################################
%% ###############################################################

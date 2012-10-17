%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Register game API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_game_api).
-export([out/1]).

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_post(A),
    Register = fun() -> game:create(Args) end,
    case request:execute(validate(), Args, Register) of
        {ok, Doc} ->
            [{status, 200}, {content, "application/json", response:ok(document:get_id(Doc))}];
        {error, developer_not_found} ->
            [{status, 404}, {content, "appllication/json", response:error("Developer not found")}];
        {error, unauthorized} ->
            [{status, 401}, {content, "appllication/json", response:error("Unauthorized")}];
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
        {"developer_id", undefined, 400, "Missing developer id"},
        {"developer_id", [], 400, "Empty developer id"},
        {"password", undefined, 400, "Missing developer password"},
        {"password", [], 400, "Empty developer password"},
        {"game_id", undefined, 400, "Missing game id"},
        {"game_id", [], 400, "Empty game id"},
        {"description", undefined, 400, "Missing description"},
        {"description", [], 400, "Empty description"}
    ].

%% ###############################################################
%% ###############################################################
%% ###############################################################

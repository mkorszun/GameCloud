%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Register game API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_game_api).
-export([out/1]).

%% ###############################################################
%% INCLUDES
%% ###############################################################

-include("api.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(GAMES, []).

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_post(A),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    Register = fun() -> register_game(DB, Args) end,
    request:execute(validate(), Args, Register).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

register_game(DB, Args) ->
    case authorization:authorize1(developer, DB, Args, true) of
        {ok, Result, DevDoc} ->
            register_game(DB, DevDoc, Args, Result);
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

register_game(DB, DevDoc, Args, true) ->
    RegisteredGames = document:read("games", DevDoc, ?GAMES),
    NewGame = document:create(parameter:delete(["developer_id", "dev_pass"], Args)),
    case document:exists(proplists:lookup("game_id", Args), RegisteredGames) of
        false ->
            NewDevDoc = document:set_value(<<"games">>, [NewGame | RegisteredGames], DevDoc),
            case database:save_doc(DB, NewDevDoc) of
                {ok, _} ->
                    [{status, 200}, {content, "text/xml", "ok"}];
                {error, _Error} ->
                    [{status, 500}, {content, "text/xml", "Internal error"}]
            end;
        true ->
            [{status, 400}, {content, "text/xml", "Game already registered"}]
    end;

register_game(_, _, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

%% ###############################################################
%% VALIDATE PARAMS
%% ###############################################################

validate() ->
    [
        {"developer_id", undefined, 404, "text/xml", "Missing developer id"},
        {"developer_id", [], 400, "text/xml", "Empty developer id"},
        {"dev_pass", undefined, 404, "text/xml", "Missing developer password"},
        {"dev_pass", [], 400, "text/xml", "Empty developer password"},
        {"game_id", undefined, 404, "text/xml", "Missing game id"},
        {"game_id", [], 400, "text/xml", "Empty game id"},
        {"description", undefined, 404, "text/xml", "Missing description"},
        {"description", [], 404, "text/xml", "Empty description"}
    ].

%% ###############################################################
%% ###############################################################
%% ###############################################################

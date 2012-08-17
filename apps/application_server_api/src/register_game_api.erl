%% @author Mateusz Korszun <mkorszun@gmail.com> 
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

-define(TYPE, {"type", "game"}).

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
    case authorization:authorize1(developer, DB, Args) of
        {ok, Result} ->
            register_game(DB, document:create([?TYPE | Args]), Result);
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

register_game(DB, Doc, true) ->
    case database:save_doc(DB, Doc) of
        {ok, _} ->
            [{status, 200}, {content, "text/xml", "ok"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end;

register_game(_, _, false) ->
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

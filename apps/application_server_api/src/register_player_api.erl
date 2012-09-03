%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
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
%% MACROS
%% ############################################################### 

-define(TYPE, {"type", "player"}).

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
    case authorization:authorize1(developer, DB, Args) of 
        {ok, Result} ->
            Params = parameter:delete(["developer_id", "dev_pass"], Args),
            register_user(DB, document:create([?TYPE | Params]), Result);
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

register_user(DB, Doc, true) ->
    case database:save_doc(DB, Doc) of
	    {ok, _} ->
	        [{status, 200}, {content, "text/xml", "ok"}]; 
	    {error, _Error} ->
	        [{status, 500}, {content, "text/xml", "Internal error"}]
    end;

register_user(_, _, false) ->
    [{status, 400}, {content, "text/xml", "Unauthorized"}].

%% ############################################################### 
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"developer_id", undefined, 404, "text/xml", "Missing developer id"},
        {"developer_id", [], 400, "text/xml", "Empty developer id"},
        {"dev_pass", undefined, 404, "text/xml", "Missing devloper password"},
        {"dev_pass", [], 404, "text/xml", "Empty developer password"},
        {"game_id", undefined, 404, "text/xml", "Missing game id"},
        {"game_id", [], 400, "text/xml", "Empty game id"},
        {"user_id", undefined, 404, "text/xml", "Missing user id"},
        {"user_id", [], 400, "text/xml", "Empty user id"},
        {"password", undefined, 404, "text/xml", "Missing user password"},
        {"password", [], 400, "text/xml", "Empty user password"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

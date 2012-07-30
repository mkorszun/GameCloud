%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud 
%%% @doc
%%% Read save API
%%% @end 
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(read_api).
-export([out/1]).

%% ###############################################################
%% CALLBACK FUNCTION 
%% ############################################################### 

-include("api.hrl").

out(A) ->
    Args = yaws_api:parse_post(A),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    Read = fun() -> read_save(DB, Args) end,
    request:execute(validate(), Args, Read).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

read_save(DB, Args) ->
    case authorization:authorize(DB, Args) of
        {ok, Result} ->
            read_save(DB, ?VIEW, [?KEYS(Args)], Result);
        {error, not_found} ->
            [{status, 404}, {content, "text/xml", "User not found"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

read_save(DB, View, Keys, true) ->
    case database:read_doc(DB, View, Keys) of
        {ok, Doc} ->
            Response = ejson:encode(Doc),
            [{status, 200}, {content, "application/json", Response}];
        {error, empty} ->
            [{status, 400}, {content, "text/xml", "Save not found"}];
        {error, _Reason} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end;

read_save(_, _, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

%% ###############################################################
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"user_id", undefined, 404, "text/xml", "Missing user name"},
        {"user_id", [], 400, "text/xml", "Empty user name"},
        {"password", undefined, 404, "text/xml", "Missing password"},
        {"password", [], 400, "text/xml", "Empty password"},
        {"game", undefined, 400, "text/xml", "Missing game name"},
        {"game", [], 400, "text/xml", "Empty game name"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

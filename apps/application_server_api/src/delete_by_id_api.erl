%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Delete save by id API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(delete_by_id_api).
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
    Delete = fun() -> delete_save(DB, Args) end,
    request:execute(validate(), Args, Delete).

%% ###############################################################
%% INTERNAL FUNCTIONS 
%% ############################################################### 

delete_save(DB, Args) ->
    case authorization:authorize1(player, DB, Args) of
        {ok, Result} ->
            {View, Keys} = views:view(delete_by_id, Args),
            delete_save(DB, View, [Keys], Result);
        {error, not_found} ->
            [{status, 404}, {content, "text/xml", "User not found"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

delete_save(DB, View, Keys, true) ->
    case database:delete_doc(DB, View, Keys) of
        {ok, _} ->
             [{status, 200}, {content, "text/xml", "ok"}];
        {error, empty} ->
             [{status, 400}, {content, "text/xml", "Save not found"}];
        {error, _Reason} ->
             [{status, 500}, {content, "text/xml", "Internal error"}]  
    end;

delete_save(_, _, _, false) ->
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
        {"id", undefined, 400, "text/xml", "Missing save id"},
        {"id", [], 400, "text/xml", "Empty save id"}
    ].

%% ############################################################### 
%% ############################################################### 
%% ############################################################### 

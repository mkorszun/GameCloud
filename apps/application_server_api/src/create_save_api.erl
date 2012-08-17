%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
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
            Create = fun() -> create_save(DB, P, F) end,
	        request:execute(validate(), P, Create);
        {error, no_multipart_form_data} ->
            {ok, DBName} = application:get_env(?APP, ?DB),
            {ok, DB} = database:open(DBName),
            Args = yaws_api:parse_post(A),
            Create = fun() -> create_save(DB, Args, []) end,
            request:execute(validate(), Args, Create);
        {error, _Reason} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

create_save(DB, Params, Files) ->
    case authorization:authorize1(player, DB, Params) of
	    {ok, Result} ->
            Doc = parameter:delete(["password"], Params),
	        create_save(DB, document:create(Doc), Files, Result);
	    {error, not_found} ->
	        [{status, 404},{content, "text/xml", "User not found"}];
	    {error, _Error} ->
	        [{status, 500}, {content, "text/xml", "Internal error"}]
    end.
     
create_save(DB, Doc, Files, true) -> 
    {View, Keys} = views:view(create, Doc),
    case couchbeam_view:fetch(DB, View, [Keys]) of
        {ok, []} ->
            {ok, Doc1} = database:save_doc(DB, Doc, Files),
            [{status, 200}, {content, "text/xml", document:get_id(Doc1)}];
        {ok, [_]} ->
            [{status, 400}, {content, "text/xml", "Save already exists"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end;

create_save(_, _, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

%% ###############################################################
%% VALIDATE PARAMS
%% ############################################################### 

validate() ->
    [
        {"user_id", undefined, 404, "text/xml", "Missing user name"},
        {"user_id", [], 404, "text/xml", "Empty user name"},
        {"password", undefined, 400, "text/xml", "Missing password"},
        {"password", [], 400, "text/xml", "Empty password"},
        {"game", undefined, 400, "text/xml", "Missing game name"},
        {"game", [], 400, "text/xml", "Empty game name"},
        {"name", undefined, 400, "text/xml", "Missing save name"},
        {"name", [], 400, "text/xml", "Empty save name"},
        {"type", undefined, 400, "text/xml", "Missing type"},
        {"type", [], 400, "text/xml", "Empty type"}
    ].
    
%% ###############################################################
%% ###############################################################
%% ############################################################### 

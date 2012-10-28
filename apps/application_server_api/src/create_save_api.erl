%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Create save API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(create_save_api).
-export([out/1]).

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
            Fun = fun() -> save:create(P, F) end,
            request(request:get_method(A), validate(), P, Fun);
        {error, no_multipart_form_data} ->
            Args = yaws_api:parse_post(A),
            Fun = fun() -> save:create(Args, []) end,
            request(request:get_method(A), validate(), Args, Fun);
        {error, _Reason} ->
            [{status, 500}, {content, "application/json", response:error("Internal error")}]
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

request('POST', ValidationList, Args, Fun) ->
    case request:execute(ValidationList, Args, Fun) of
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
        {"password", undefined, 400, "Missing player password"},
        {"password", [], 400, "Empty player password"},
        {"save_name", undefined, 400, "Missing save name"},
        {"save_name", [], 400, "Empty save name"},
        {"date", undefined, 400, "Missing date"},
        {"date", [], 400, "Empty date"}
    ].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

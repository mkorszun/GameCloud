%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud 
%%% @doc
%%% Register developer API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_developer_api).
-export([out/1]).

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_post(A),
    Fun = fun() -> developer:create(Args) end,
    request(request:get_method(A), validate(), Args, Fun).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

request('POST', ValidationList, Args, Fun) ->
    case request:execute(ValidationList, Args, Fun) of
        {ok, _} ->
            [{status, 200}, {content, "application/json", response:ok("ok")}];
        {error, developer_already_exists} ->
            [{status, 403}, {content, "appllication/json", response:error("Developer already exists")}];
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
        {"email", undefined, 400, "Missing email"},
        {"email", [], 400, "Empty email"}
    ].

%% ###############################################################
%% ###############################################################
%% ###############################################################

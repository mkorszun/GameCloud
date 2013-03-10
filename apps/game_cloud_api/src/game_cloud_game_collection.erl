%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game collection resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_game_collection).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([process_post/2, to_json/2, to_html/2, forbidden/2, is_authorized/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}, {"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

process_post(ReqData, State) ->
    DeveloperId = dict:fetch(developer, wrq:path_info(ReqData)),
    try game_cloud_api_utils:request_body(ReqData) of
        Game ->
            case game:create([{<<"developer_id">>, list_to_binary(DeveloperId)} | Game]) of
                {ok, Doc} ->
                    Id = document:get_id(Doc),
                    Response = mochijson2:encode([{game_key, Id}]),
                    ReqData1 = game_cloud_api_utils:set_location(Id, ReqData),
                    {true, wrq:set_resp_body(Response, ReqData1), State};
                {error, {bad_data, Reason}} ->
                    ?ERR("Failed to create game for developer id=~s, bad data: ~p." ++
                        " Request data: ~p", [DeveloperId, Reason, Game]),
                    Msg = game_cloud_api_errors:error(Reason),
                    {{halt, 400}, wrq:set_resp_body(Msg, ReqData) , State};
                {error, Error} ->
                    ?ERR("Failed to create game for developer id=~s: ~p." ++
                        " Request data: ~p", [DeveloperId, Error, Game]),
                    {{halt, 500}, ReqData, State}
            end
    catch
        _:Reason ->
            ?ERR("Failed to create game for developer id=~s, bad data: ~p",
                [DeveloperId, Reason]),
            {{halt, 400}, ReqData, State}
    end.

%% ###############################################################
%% READ
%% ###############################################################

to_json(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    Exclude = wrq:get_qs_value("exclude", ReqData),
    ExcludeList = game_cloud_api_utils:decode_list(Exclude),
    case developer:list_games(DeveloperId, ExcludeList) of
        {ok, Games} ->
            {mochijson2:encode(Games), ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read games for developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

to_html(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    Exclude = wrq:get_qs_value("exclude", ReqData),
    ExcludeList = game_cloud_api_utils:decode_list(Exclude),
    case developer:list_games(DeveloperId, ExcludeList) of
        {ok, Games} ->
            {game_cloud_api_utils:build_page(Games), ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read games for developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

%% ###############################################################
%%
%% ###############################################################

forbidden(#wm_reqdata{method = 'GET'} = ReqData, State) ->
    case wrq:get_req_header("accept", ReqData) of
        "text/html" -> {false, ReqData, State};
        _ -> do_forbidden(ReqData, State)
    end;
forbidden(#wm_reqdata{method = 'POST'} = ReqData, State) ->
    do_forbidden(ReqData, State).

is_authorized(#wm_reqdata{method = 'GET'} = ReqData, State) ->
    case wrq:get_req_header("accept", ReqData) of
        "text/html" -> {true, ReqData, State};
        _ -> game_cloud_api_auth:is_authorized(developer, ReqData, State)
    end;
is_authorized(#wm_reqdata{method = 'POST'} = ReqData, State) ->
    game_cloud_api_auth:is_authorized(developer, ReqData, State).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_forbidden(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    User = proplists:get_value(user, State),
    {(DeveloperId == User) == false, ReqData, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################
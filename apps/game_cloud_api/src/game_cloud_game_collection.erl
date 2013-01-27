%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game collection resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_game_collection).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, post_is_create/2,
    content_types_provided/2, content_types_accepted/2]).

-export([to_json/2, to_html/2, create_path/2, forbidden/2, is_authorized/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}, {"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

to_json(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    case developer:list_games(DeveloperId) of
        {ok, Games} ->
            Response = mochijson2:encode(Games),
            {Response, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read games for developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

to_html(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    case developer:list_games(DeveloperId) of
        {ok, Games} ->
            Page = game_cloud_api_utils:build_page(DeveloperId, Games),
            {Page, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read games for developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

create_path(ReqData, State) ->
    Id = proplists:get_value(<<"id">>, State),
    Response = mochijson2:encode([{game_key, Id}]),
    {binary_to_list(Id), wrq:set_resp_body(Response, ReqData), State}.


forbidden(#wm_reqdata{method = 'GET'} = ReqData, State) ->
    {false, ReqData, State};
forbidden(#wm_reqdata{method = 'POST'} = ReqData, State) ->
    DeveloperId = dict:fetch(developer, wrq:path_info(ReqData)),
    try game_cloud_api_utils:get_request_body(ReqData, State, game) of
        {Game, NewState} ->
            case game:create([{<<"developer_id">>, list_to_binary(DeveloperId)} | Game]) of
                {ok, Doc} ->
                    {false, ReqData, [{<<"id">>, document:get_id(Doc)} | NewState]};
                {error, {bad_data, Reason}} ->
                    ?ERR("Failed to create game for developer id=~s, bad data: ~p",
                        [DeveloperId, Reason]),
                    {{halt, 400}, ReqData, State};
                {error, Error} ->
                    ?ERR("Failed to create game for developer id=~s: ~p",
                        [DeveloperId, Error]),
                    {{halt, 500}, ReqData, State}
            end
    catch
        _:Reason ->
            io:format("baam"),
            ?ERR("Failed to create game for developer id=~s, bad data: ~p",
                [DeveloperId, Reason]),
            {{halt, 400}, ReqData, State}
    end.

is_authorized(ReqData, State) ->
    game_cloud_api_utils:is_authorized(developer, ReqData, State).

%% ###############################################################
%% ###############################################################
%% ###############################################################
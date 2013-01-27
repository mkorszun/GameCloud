%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_game).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([process_post/2, to_json/2, delete_resource/2, forbidden/2, is_authorized/2]).

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
    {['GET', 'POST', 'DELETE'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

process_post(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    GameId = dict:fetch(game, Data),
    try game_cloud_api_utils:get_request_body(ReqData, State, game) of
        {Game, NewState} ->
            case game:update(DeveloperId, GameId, Game) of
                {ok, _} ->
                    {true, ReqData, NewState};
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
            ?ERR("Failed to create game for developer id=~s, bad data: ~p",
                [DeveloperId, Reason]),
            {{halt, 400}, ReqData, State}
    end.

to_json(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    GameId = dict:fetch(game, Data),
    case game:read(DeveloperId, GameId) of
        {ok, Doc} ->
            Response = mochijson2:encode(Doc),
            {Response, ReqData, State};
        {error, not_found} ->
            ?ERR("Failed to read game id=~s for developer id=~s: not found",
                [GameId, DeveloperId]),
            {{halt, 404}, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read game id=~s for developer id=~s: ~p",
                [GameId, DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

delete_resource(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    GameId = dict:fetch(game, Data),
    case game:delete(DeveloperId, GameId) of
        {ok, _} ->
            {true, ReqData, State};
        {error, not_found} ->
            ?ERR("Failed to delete game id=~s for developer id=~s: not found",
                [GameId, DeveloperId]),
            {{halt, 404}, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read game id=~s for developer id=~s: ~p",
                [GameId, DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

forbidden(ReqData, State) ->
    Data = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Data),
    User = proplists:get_value(user, State),
    {(DeveloperId == User) == false, ReqData, State}.

is_authorized(ReqData, State) ->
    game_cloud_api_utils:is_authorized(developer, ReqData, State).

%% ###############################################################
%% ###############################################################
%% ###############################################################
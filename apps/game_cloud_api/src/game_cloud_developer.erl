%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_developer).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, from_json/2, delete_resource/2, forbidden/2, is_authorized/2]).

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
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", from_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

to_json(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    case developer:read(DeveloperId) of
        {ok, Doc} ->
            {mochijson2:encode(Doc), ReqData, State};
        {error, not_found} ->
            ?ERR("Failed to read developer id=~s: not found",
                [DeveloperId]),
            {{halt, 404}, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

%% ###############################################################
%% UPDATE
%% ###############################################################

from_json(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    try game_cloud_api_utils:request_body(ReqData) of
        {struct, Developer} ->
            case developer:update(DeveloperId, Developer) of
                {ok, _} ->
                    {true, ReqData, State};
                {error, not_found} ->
                    ?ERR("Failed to update developer id=~s: not found." ++
                        " Request data: ~p", [DeveloperId, Developer]),
                    {{halt, 404}, ReqData, State};
                {error, Error} ->
                    ?ERR("Failed to update developer id=~s: ~p." ++
                       " Request data: ~p", [DeveloperId, Error, Developer]),
                    {{halt, 500}, ReqData, State}
            end
    catch
        _:Reason ->
            ?ERR("Failed to update developer, bad data: ~p", [Reason]),
            {{halt, 400}, ReqData, State}
    end.

%% ###############################################################
%% DELETE
%% ###############################################################

delete_resource(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    case developer:delete(DeveloperId) of
        {ok, _} ->
            {true, ReqData, State};
        {error, not_found} ->
            ?ERR("Failed to delete developer id=~s: not found",
                [DeveloperId]),
            {{halt, 404}, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to delete developer id=~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

%% ###############################################################
%%
%% ###############################################################

forbidden(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    User = proplists:get_value(user, State),
    {(DeveloperId == User) == false, ReqData, State}.

is_authorized(ReqData, State) ->
    game_cloud_api_utils:is_authorized(developer, ReqData, State).

%% ###############################################################
%% ###############################################################
%% ###############################################################
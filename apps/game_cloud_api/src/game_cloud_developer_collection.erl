%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer collection resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_developer_collection).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, post_is_create/2,
    content_types_provided/2, content_types_accepted/2]).

-export([to_json/2, create_path/2, forbidden/2]).

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
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% REQUEST
%% ###############################################################

to_json(ReqData, State) ->
    {ok, ReqData, State}.

create_path(ReqData, State) ->
    {Developer, NewState} = game_cloud_api_utils:
        get_request_body(ReqData, State, developer),
    Id = proplists:get_value(<<"id">>, Developer),
    {binary_to_list(Id), ReqData, NewState}.

forbidden(ReqData, State) ->
    try game_cloud_api_utils:get_request_body(ReqData, State, developer) of
        {Developer, NewState} ->
            case developer:create(Developer) of
                {ok, _} ->
                    {false, ReqData, NewState};
                {error, conflict} ->
                    {true, ReqData, State};
                {error, {bad_data, Reason}} ->
                    ?ERR("Failed to create developer, bad data: ~p", [Reason]),
                    {{halt, 400}, ReqData, State};
                {error, Error} ->
                    ?ERR("Failed to create developer: ~p", [Error]),
                    {{halt, 500}, ReqData, State}
            end
    catch
        _:Reason ->
            ?ERR("Failed to create developer, bad data: ~p", [Reason]),
            {{halt, 400}, ReqData, State}
    end.

%% ###############################################################
%%
%% ###############################################################
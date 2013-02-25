%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Token resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_token).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2]).
-export([to_json/2, is_authorized/2]).

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
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% REQUEST
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

to_json(ReqData, State) ->
    DeveloperId = proplists:get_value(user, State),
    case token:create(list_to_binary(DeveloperId)) of
        {ok, Doc} ->
            Token = document:read(<<"token">>, Doc),
            Response = mochijson2:encode({[{token, Token}]}),
            {Response, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to create token for developer id = ~s: ~p",
                [DeveloperId, Error]),
            {{halt, 500}, ReqData, State}
    end.

is_authorized(ReqData, State) ->
    game_cloud_api_auth:is_authorized(developer, ReqData, State, basic).

%% ###############################################################
%% ###############################################################
%% ###############################################################
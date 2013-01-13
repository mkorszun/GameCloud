%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_developer).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, delete_resource/2, is_authorized/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

to_json(ReqData, State) ->
	Data = wrq:path_info(ReqData),
	ResourceId = dict:fetch(developer, Data),
	User = proplists:get_value(user, State),
	if
		ResourceId == User ->
			case developer:read(ResourceId) of
				{ok, Doc} ->
					Response = mochijson2:encode(Doc),
					{Response, ReqData, State};
				{error, not_found} ->
					?ERR("Failed to read developer id=~s: not found", 
						[ResourceId]),
					{{halt, 404}, ReqData, State};
				{error, Error} ->
					?ERR("Failed to read developer id=~s: ~s", 
						[ResourceId, Error]),
					{{halt, 500}, ReqData, State}
			end;
		true ->
			?ERR("Developer id=~s not authorized to access ~s", 
				[ResourceId, User]),
			{{halt, 403}, ReqData, State}	
	end.

delete_resource(ReqData, State) ->
	Data = wrq:path_info(ReqData),
	ResourceId = dict:fetch(developer, Data),
	User = proplists:get_value(user, State),
	if 
		ResourceId == User -> 
			case developer:delete(ResourceId) of
				{ok, _} ->
					{true, ReqData, State};
				{error, not_found} ->
					?ERR("Failed to delete developer id=~s: not found", 
						[ResourceId]),
					{{halt, 404}, ReqData, State};
				{error, Error} ->
					?ERR("Failed to delete developer id=~s: ~s", 
						[ResourceId, Error]),
					{{halt, 500}, ReqData, State}
			end;
		true ->
			?ERR("Developer id=~s not authorized to access ~s", 
				[ResourceId, User]),
			{{halt, 403}, ReqData, State}
	end.

is_authorized(ReqData, State) ->
    game_cloud_api_utils:is_authorized(developer, ReqData, State).

%% ###############################################################
%%
%% ###############################################################
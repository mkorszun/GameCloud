%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player collection resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_player_collection).

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

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

to_json(ReqData, State) ->
	Data = wrq:path_info(ReqData),
	GameId = dict:fetch(game, Data),
	DeveloperId = dict:fetch(developer, Data),
	case game:list_players(DeveloperId, GameId) of
		{ok, Players} ->
			Response = mochijson2:encode(Players),
			{Response, ReqData, State};
		{error, _} ->
			{{halt, 500}, ReqData, State}
	end.

create_path(ReqData, State) ->
	Id = proplists:get_value(<<"id">>, State),
	{binary_to_list(Id), ReqData, State}.


forbidden(#wm_reqdata{method = 'GET'} = ReqData, State) ->
	{false, ReqData, State};
forbidden(#wm_reqdata{method = 'POST'} = ReqData, State) ->
	try game_cloud_api_utils:get_request_body(ReqData, State, player) of
		{Player, NewState} ->
			Data = wrq:path_info(ReqData),
			DeveloperId = dict:fetch(developer, Data),
			GameKey = dict:fetch(game, Data),
			case player:create([{<<"developer_id">>, DeveloperId}, {<<"game_key">>, GameKey} | Player]) of
				{ok, Doc} ->
					{false, ReqData, [{<<"id">>, document:get_id(Doc)}]};
				{error, conflict} ->
					{{halt, 409}, ReqData, State};
				{error, not_found} ->
					{{halt, 404}, ReqData, State};
				{error, bad_data} ->
					{{halt, 400}, ReqData, State};
				{error, _} ->
					{{halt, 500}, ReqData, State}
			end
	catch
		_:_ ->
			{{halt, 400}, ReqData, State}
	end.

%% ###############################################################
%% 
%% ###############################################################
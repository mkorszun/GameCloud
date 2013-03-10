%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer collection resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_developer_collection).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, known_content_type/2]).
-export([process_post/2]).

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
    {['POST'], ReqData, Context}.

known_content_type(ReqData, Context) ->
    case wrq:get_req_header("content-type", ReqData) of
        "application/json" ->
            {true, ReqData, Context};
        _ ->
            {false, ReqData, Context}
    end.

%% ###############################################################
%% REQUEST
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

process_post(ReqData, State) ->
    try game_cloud_api_utils:request_body(ReqData) of
        Developer ->
            case developer:create(Developer) of
                {ok, Doc} ->
                    {true, game_cloud_api_utils:set_location(
                        document:get_id(Doc), ReqData), State};
                {error, conflict} ->
                    ?ERR("Failed to create developer: already exists." ++
                        " Request data: ~p", [Developer]),
                    {{halt, 409}, ReqData, State};
                {error, {bad_data, Reason}} ->
                    ?ERR("Failed to create developer, bad data: ~p." ++
                        " Request data: ~p", [Reason, Developer]),
                    Msg = game_cloud_api_errors:error(Reason),
                    {{halt, 400}, wrq:set_resp_body(Msg, ReqData) , State};
                {error, Error} ->
                    ?ERR("Failed to create developer: ~p. Request data: ~p",
                        [Error, Developer]),
                    {{halt, 500}, ReqData, State}
            end
    catch
        _:Reason ->
            ?ERR("Failed to create developer, bad data: ~p", [Reason]),
            {{halt, 400}, ReqData, State}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
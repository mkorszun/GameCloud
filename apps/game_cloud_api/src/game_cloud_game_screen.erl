%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game screen resource
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_game_screen).

-compile([{parse_transform, lager_transform}]).

-export([init/1, allowed_methods/2, content_types_provided/2]).
-export([provide_content/2]).

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

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    CT = webmachine_util:guess_mime(wrq:path(ReqData)),
    {[{CT, provide_content}], ReqData, State}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

provide_content(ReqData, State) ->
    Path = wrq:path_info(ReqData),
    DeveloperId = dict:fetch(developer, Path),
    GameId = dict:fetch(game, Path),
    FileName = dict:fetch(screen, Path),
    case game:read_screen(DeveloperId, GameId, FileName) of
        {ok, Doc} ->
            Type = binary_to_list(document:read(<<"content_type">>, Doc)),
            Content = document:read(<<"content">>, Doc),
            NewReqData = wrq:set_resp_header("content-type", Type, ReqData),
            {base64:decode(Content), NewReqData, State};
        {error, not_found} ->
            ?ERR("Failed to read game screen=~s for developer id=~s" ++
                " and game=~s: not_found", [FileName, DeveloperId, GameId]),
            {{halt, 404}, ReqData, State};
        {error, Error} ->
            ?ERR("Failed to read game screen=~s for developer id=~s" ++
                " and game=~s: ~p", [FileName, DeveloperId, GameId, Error]),
            {{halt, 500}, ReqData, State}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
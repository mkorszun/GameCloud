%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-compile([{parse_transform, lager_transform}]).

-export([create/1, create/2]).
-export([exists1/3, exists1/4, exists2/2, exists2/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "game"}).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(Args) ->
    create(application_server_db:connection(), Args).

create(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    Password = proplists:get_value("password", Args),
    GameName = proplists:get_value("game_id", Args),
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            ?DBG("Developer=~s authorized, registering game=~s",
                [DeveloperId, GameName]),
            database:save_doc(DB, build_doc(Args));
        {error, Error} ->
            ?ERR("Failed to create game name=~s for developer=~s: ~p",
                [GameName, DeveloperId, Error]),
            {error, Error}
    end.

%% ###############################################################
%% EXISTS
%% ###############################################################

exists1(DeveloperId, Password, GameKey) ->
    exists1(application_server_db:connection(), DeveloperId, Password, GameKey).

exists1(DB, DeveloperId, Password, GameKey) ->
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            ?DBG("Developer=~s authorized, checking game=~s",
                [DeveloperId, GameKey]),
            exists2(DB, DeveloperId, GameKey);
        {error, Error} ->
            ?ERR("Failed to find game=~s for developer=~s: ~p",
                [GameKey, DeveloperId, Error]),
            {error, Error}
    end.

exists2(DeveloperId, GameKey) ->
    exists2(application_server_db:connection(), DeveloperId, GameKey).

exists2(DB, DeveloperId, GameKey) ->
    View = {<<"games">>, <<"by_developer">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            ?INF("Game=~s for developer=~s exists",
                [GameKey, DeveloperId]),
            {ok, true};
        {error, not_found} ->
            ?ERR("Failed to find game=~s for developer=~s",
                [GameKey, DeveloperId]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to find game=~s for developer=~s: ~p",
                [GameKey, DeveloperId, Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Args) ->
    DeveloperId = proplists:lookup("developer_id", Args),
    GameId = proplists:lookup("game_id", Args),
    Description = proplists:lookup("description", Args),
    document:create([DeveloperId, GameId, Description, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
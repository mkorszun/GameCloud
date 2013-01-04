%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).

-compile([{parse_transform, lager_transform}]).

-export([create/1, create/2]).
-export([exists/2, exists/3]).
-export([authorize/3, authorize/4]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% MACORS
%% ############################################################### 

-define(TYPE, {"type", "player"}).

%% ###############################################################
%% API
%% ############################################################### 

create(Args) ->
    create(application_server_db:connection(), Args).

create(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    DevPassword = proplists:get_value("dev_password", Args),
    GameKey = proplists:get_value("game_key", Args),
    PlayerId = proplists:get_value("player_id", Args),
    case game:exists1(DB, DeveloperId, DevPassword, GameKey) of
        {ok, true} ->
            ?DBG("Game=~s exists, creating player=~s",
                [GameKey, PlayerId]),
            do_register(DB, Args);
        {error, Error} ->
            ?ERR("Failed to create player=~s for game=~s: ~p",
                [PlayerId, GameKey, Error]),
            {error, Error}
    end.

%% ###############################################################
%% AUTHORIZE
%% ############################################################### 

authorize(GameKey, PlayerId, Password) ->
    authorize(application_server_db:connection(), GameKey, PlayerId, Password).

authorize(DB, GameKey, PlayerId, Password) ->
    View = {<<"players">>, <<"by_game">>},
    Keys = {key, views:keys([GameKey, PlayerId])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, [Doc]} ->
            ?DBG("Player=~s found for game=~s, authorizing",
                [PlayerId, GameKey]),
            authorization:authorize(player, Doc, PlayerId, Password);
        {ok, []} ->
            ?ERR("Failed to find player=~s for game=~s",
                [PlayerId, GameKey]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to find player=~s for game=~s: ~p",
                [PlayerId, GameKey, Error]),
            {error, Error}
    end.

%% ###############################################################
%% EXISTS
%% ############################################################### 

exists(GameKey, PlayerId) ->
    exists(application_server_db:connection(), GameKey, PlayerId).

exists(DB, GameKey, PlayerId) ->
    View = {<<"players">>, <<"by_game">>},
    Keys = {key, views:keys([GameKey, PlayerId])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            ?DBG("Player=~s found for game=~s",
                [PlayerId, GameKey]),
            {ok, true};
        {error, not_found} ->
            ?ERR("Failed to find player=~s for game=~s",
                [PlayerId, GameKey]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to find player=~s for game=~s: ~p",
                [PlayerId, GameKey, Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_register(DB, Args) ->
    PlayerId = proplists:get_value("player_id", Args),
    GameKey = proplists:get_value("game_key", Args),
    case database:save_doc(DB, build_doc(Args)) of
        {ok, Doc} ->
            ?DBG("Player=~s created for game=~s",
                [PlayerId, GameKey]),
            {ok, Doc};
        {error, conflict} ->
            ?ERR("Player=~s already exists for game=~s",
                [PlayerId, GameKey]),
            {error, player_already_exists};
        {error, Error} ->
            ?DBG("Failed to create player=~s for game=~s",
                [PlayerId, GameKey]),
            {error, Error}
    end.

build_doc(Args) ->
    GameUUID = proplists:get_value("game_key", Args),
    PlayerId = proplists:get_value("player_id", Args),
    Password = proplists:get_value("password", Args),
    PasswordHash = cryptography:sha(Password, PlayerId),
    document:create([{"game_key", GameUUID}, {"_id", PlayerId}, 
        {"password", PasswordHash}, {"type", "player"}]).

%% ###############################################################
%% 
%% ############################################################### 
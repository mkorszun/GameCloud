%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-compile([{parse_transform, lager_transform}]).

-export([create/1, create/2]).
-export([list_players/1, list_players/2]).
-export([delete_players/1, delete_players/2]).
-export([exists1/3, exists1/4, exists2/2, exists2/3]).
-export([delete1/1, delete1/2, delete2/3, delete2/4, delete3/1, delete3/2]).

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

exists1(DeveloperId, Password, GameUUID) ->
    exists1(application_server_db:connection(), DeveloperId, Password, GameUUID).

exists1(DB, DeveloperId, Password, GameUUID) ->
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            ?DBG("Developer=~s authorized, checking game=~s",
                [DeveloperId, GameUUID]),
            exists2(DB, DeveloperId, GameUUID);
        {error, Error} ->
            ?ERR("Failed to find game=~s for developer=~s: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

exists2(DeveloperId, GameUUID) ->
    exists2(application_server_db:connection(), DeveloperId, GameUUID).

exists2(DB, DeveloperId, GameUUID) ->
    View = {<<"games">>, <<"by_developer">>},
    Keys = {key, views:keys([DeveloperId, GameUUID])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            ?INF("Game=~s for developer=~s exists",
                [GameUUID, DeveloperId]),
            {ok, true};
        {error, not_found} ->
            ?ERR("Failed to find game=~s for developer=~s",
                [GameUUID, DeveloperId]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to find game=~s for developer=~s: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

%% ###############################################################
%% DELETE
%% ###############################################################

delete1(Args) ->
    delete1(application_server_db:connection(), Args).

delete1(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    Password = proplists:get_value("password", Args),
    GameUUID = proplists:get_value("game_uuid", Args),
    delete2(DB, DeveloperId, Password, GameUUID).

delete2(DeveloperId, Password, GameUUID) ->
    delete2(application_server_db:connection(), DeveloperId, Password, GameUUID).

delete2(DB, DeveloperId, Password, GameUUID) ->
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            ?DBG("Developer=~s authorized, deleting game=~s",
                [DeveloperId, GameUUID]),
            delete3(DB, GameUUID);
        {error, Error} ->
            ?ERR("Failed to delete game=~s for developer=~s: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

delete3(GameUUID) ->
    delete3(application_server_db:connection(), GameUUID).

delete3(DB, GameUUID) ->
    {ok, _} = delete_players(DB, GameUUID),
    case database:delete_doc(DB, GameUUID) of
        {ok, Doc} ->
            ?INF("Game=~s deleted", [GameUUID]),
            {ok, Doc};
        {error, not_found} ->
            ?ERR("Game=~s not found", [GameUUID]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to delete game=~s: ~p",
                [GameUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% LIST PLAYERS
%% ###############################################################

list_players(GameUUID) ->
    list_players(application_server_db:connection(), GameUUID).

list_players(DB, GameUUID) ->
    View = {<<"players">>, <<"all">>},
    Keys = {key, views:keys([GameUUID])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% DELETE PLAYERS
%% ###############################################################

delete_players(GameUUID) ->
    delete_players(application_server_db:connection(), GameUUID).

delete_players(DB, GameUUID) ->
    case list_players(DB, GameUUID) of
        {ok, Players} ->
            ?DBG("Deleting ~p players for game=~s",
                [length(Players), GameUUID]),
            {ok, [player:delete3(DB, X) || X <- Players]};
        {error, Error} ->
            ?ERR("Failed to get players for game=~s: ~p",
                [GameUUID, Error]),
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
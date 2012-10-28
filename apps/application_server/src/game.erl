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
            database:save_doc(DB, build_doc(Args));
        {error, Error} ->
            ?ERR("Failed to create game name=~p for developer=~p: ~p", 
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
            exists2(DB, DeveloperId, GameUUID);
        {error, Error} ->
            ?ERR("Failed to find game=~p for developer=~p: ~p",
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
            {ok, true};
        {error, not_found} ->
            ?ERR("Failed to find game=~p for developer=~p",
                [GameUUID, DeveloperId]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to find game=~p for developer=~p: ~p",
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
            delete3(DB, GameUUID);
        {error, Error} ->
            ?ERR("Failed to delete game=~p for developer=~p: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

delete3(GameUUID) ->
    delete3(application_server_db:connection(), GameUUID).

delete3(DB, GameUUID) ->
    {ok, Players} = list_players(DB, GameUUID),
    [player:delete3(DB, X) || X <- Players],
    case database:delete_doc(DB, GameUUID) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            ?ERR("Game=~p not found", [GameUUID]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to delete game=~p: ~p",
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
%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game save business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(save).

-compile([{parse_transform, lager_transform}]).

-export([create/2, create/3]).
-export([read1/1, read1/2, read2/1, read2/2]).
-export([list1/1, list1/2, list2/1, list2/2]).
-export([delete1/1, delete1/2, delete2/3, delete2/4, delete3/2, delete3/3, delete4/1, delete4/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% MACROS
%% ############################################################### 

-define(TYPE, {"type", "save"}).

%% ###############################################################
%% API
%% ############################################################### 

%% ###############################################################
%% CREATE
%% ############################################################### 

create(Args, Files) ->
    create(application_server_db:connection(), Args, Files).

create(DB, Args, Files) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, creating save",
                [PlayerUUID]),
            database:save_doc(DB, build_doc(Args), Files);
        {error, Error} ->
            ?ERR("Failed to create save for player=~s: ~p",
                [PlayerUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% READ
%% ############################################################### 

read1(Args) ->
    read1(application_server_db:connection(), Args).

read1(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    SaveUUID = proplists:get_value("save_uuid", Args),
    case player:authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, reading save=~s",
                [PlayerUUID, SaveUUID]),
            read2(DB, SaveUUID);
        {error, Error} ->
            ?ERR("Failed to read save=~s for player=~s: ~p",
                [SaveUUID, PlayerUUID, Error]),
            {error, Error}
    end.

read2(SaveUUID) ->
    read2(application_server_db:connection(), SaveUUID).

read2(DB, SaveUUID) ->
    case database:read_doc(DB, SaveUUID) of
        {ok, Doc} ->
            ?DBG("Save=~s found, getting attachments",
                [SaveUUID]),
            {ok, attachments:get(DB, Doc, true)};
        {error, not_found} ->
            ?DBG("Failed to find save=~s",
                [SaveUUID]),
            {error, save_not_found};
        {error, Error} ->
            ?DBG("Failed to find save=~s: ~p",
                [SaveUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% LIST
%% ############################################################### 

list1(Args) ->
    list1(application_server_db:connection(), Args).

list1(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, listing saves",
                [PlayerUUID]),
            list2(DB, PlayerUUID);
        {error, Error} ->
            ?ERR("Failed to list saves for player=~s: ~p",
                [PlayerUUID, Error]),
            {error, Error}
    end.

list2(PlayerUUID) ->
    list2(application_server_db:connection(), PlayerUUID).

list2(DB, PlayerUUID) ->
    View = {<<"game_saves">>, <<"player_game">>},
    Keys = {key, views:keys([PlayerUUID])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% DELETE
%% ###############################################################

delete1(Args) ->
    delete1(application_server_db:connection(), Args).

delete1(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    SaveUUID = proplists:get_value("save_uuid", Args),
    delete2(DB, PlayerUUID, Password, SaveUUID).

delete2(PlayerUUID, Password, SaveUUID) ->
    delete2(application_server_db:connection(), PlayerUUID, Password, SaveUUID).

delete2(DB, PlayerUUID, Password, SaveUUID) ->
    case player:authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, deleting save=~s",
                [PlayerUUID, SaveUUID]),
            delete3(DB, PlayerUUID, SaveUUID);
        {error, Error} ->
            ?ERR("Failed to delete save=~s for player=~s: ~p",
                [SaveUUID, PlayerUUID, Error]),
            {error, Error}
    end.

delete3(PlayerUUID, SaveUUID) ->
    delete3(application_server_db:connection(), PlayerUUID, SaveUUID).

delete3(DB, PlayerUUID, SaveUUID) ->
    View = {<<"game_saves">>, <<"player_save">>},
    Keys = {key, views:keys([PlayerUUID, SaveUUID])},
    case database:delete_doc(DB, View, [Keys]) of
        {ok, [Doc]} ->
            ?INF("Save=~s for player=~s deleted",
                [SaveUUID, PlayerUUID]),
            {ok, Doc};
        {error, not_found} ->
            ?ERR("Save=~s for player=~s not found",
                [SaveUUID, PlayerUUID]),
            {error, save_not_found};
        {error, Error} ->
            ?ERR("Failed to delete save=~s for player=~s: ~p",
                [SaveUUID, PlayerUUID, Error]),
            {error, Error}
    end.

delete4(SaveUUID) ->
    delete4(application_server_db:connection(), SaveUUID).

delete4(DB, SaveUUID) ->
    case database:delete_doc(DB, SaveUUID) of
        {ok, Doc} ->
            ?INF("Save=~s deleted", [SaveUUID]),
            {ok, Doc};
        {error, not_found} ->
            ?ERR("Save=~s not found", [SaveUUID]),
            {error, save_not_found};
        {error, Error} ->
            ?ERR("Failed to delete save=~s: ~p",
                [SaveUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 
 
build_doc(Args) ->
    PlayerUUID = proplists:lookup("player_uuid", Args),
    SaveName = proplists:lookup("save_name", Args),
    Date = proplists:lookup("date", Args),
    document:create([PlayerUUID, SaveName, Date, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
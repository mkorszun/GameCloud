%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game save business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(save).

-compile([{parse_transform, lager_transform}]).

-export([create/2, read/1, delete/1]).
-export([create/3, read/2, delete/2]).

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

create(Args, Files) ->
    create(application_server_db:connection(), Args, Files).

create(DB, Args, Files) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    GameUUID = proplists:get_value("game_uuid", Args),
    case player:authorize_game(DB, PlayerUUID, Password, GameUUID) of
        {ok, true} ->
             do_register(DB, build_doc(Args), Files);
        {error, Error} ->
            ?ERR("Failed to create save for game=~p and player=~p: ", 
                [GameUUID, PlayerUUID, Error]),
            {error, Error}
    end.

read(Args) ->
    read(application_server_db:connection(), Args).

read(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    SaveUUID = proplists:get_value("save_uuid", Args),
    case player:authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_read(DB, SaveUUID);
        {error, Error} ->
            ?ERR("Failed to read save=~p for player=~p: ~p", 
                [SaveUUID, PlayerUUID, Error]),
            {error, Error}
    end.

delete(Args) ->
    delete(application_server_db:connection(), Args).

delete(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    SaveUUID = proplists:get_value("save_uuid", Args),
    case player:authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_delete(DB, SaveUUID);
        {error, Error} ->
            ?ERR("Failed to delete save=~p for player=~p: ~p", 
                [SaveUUID, PlayerUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_register(DB, Doc, Files) ->
    database:save_doc(DB, Doc, Files).

do_read(DB, SaveUUID) ->
    case database:read_doc(DB, SaveUUID) of
        {ok, Doc} ->
            {ok, attachments:get(DB, Doc, true)};
        {error, not_found} ->
            {error, save_not_found};
        {error, Error} ->
            {error, Error}
    end.

do_delete(DB, SaveUUID) ->
    case database:delete_doc(DB, SaveUUID) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            {error, save_not_found};
        {error, Error} ->
            {error, Error}
    end.

build_doc(Args) ->
    PlayerUUID = proplists:lookup("player_uuid", Args),
    GameUUID = proplists:lookup("game_uuid", Args),
    SaveName = proplists:lookup("save_name", Args),
    document:create([PlayerUUID, GameUUID, SaveName, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
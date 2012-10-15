%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game save business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(save).
-export([register/3, read/2, delete/2]).

%% ###############################################################
%% MACROS
%% ############################################################### 

-define(TYPE, {"type", "save"}).

%% ###############################################################
%% API
%% ############################################################### 

register(DB, Args, Files) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    GameUUID = proplists:get_value("game_uuid", Args),
    case player:authorize(DB, PlayerUUID, Password, GameUUID) of
        {ok, true} ->
             do_register(DB, build_doc(Args), Files);
        {error, Error} ->
            {error, Error}
    end.

read(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_read(DB, proplists:get_value("save_uuid", Args));
        {error, Error} ->
            {error, Error}
    end.

delete(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_delete(DB, proplists:get_value("save_uuid", Args));
        {error, Error} ->
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
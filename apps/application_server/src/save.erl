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
    Fun = fun() -> do_register(DB, Args, Files) end,
    do_authorize_and_execute(DB, Args, Fun).

read(DB, Args) ->
    Fun = fun() -> do_read(DB, proplists:get_value("save_uuid", Args)) end,
    do_authorize_and_execute(DB, Args, Fun).

delete(DB, Args) ->
    Fun = fun() -> do_delete(DB, proplists:get_value("save_uuid", Args)) end,
    do_authorize_and_execute(DB, Args, Fun).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_authorize_and_execute(DB, Args, Fun) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            Fun();
        {error, Error} ->
            {error, Error}
    end.

do_register(DB, Args, Files) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    GameUUID = proplists:get_value("game_uuid", Args),
    case player:exists(DB, PlayerUUID, Password, GameUUID) of
        {ok, true} ->
            database:save_doc(DB, build_doc(Args), Files);
        {error, Error} ->
            {error, Error}
    end.

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
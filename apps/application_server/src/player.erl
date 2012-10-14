%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).
-export([register/2, delete/2, authorize/3, exists/4]).

%% ###############################################################
%% MACORS
%% ############################################################### 

-define(TYPE, {"type", "player"}).

%% ###############################################################
%% API
%% ############################################################### 

register(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    DevPassword = proplists:get_value("dev_password", Args),
    GameUUID = proplists:get_value("game_uuid", Args),
    case game:exists(DB, DeveloperId, DevPassword, GameUUID) of
        {ok, true} ->
            do_register(DB, Args);
        {error, Error} ->
            {error, Error}
    end.

delete(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_delete(DB, Args);
        {error, Error} ->
            {error, Error}
    end.

authorize(DB, PlayerUUID, Password) ->
    case database:read_doc(DB, PlayerUUID) of
        {ok, Doc} ->
            authorization:authorize(player, Doc, PlayerUUID, Password);
        {error, not_found} ->
            {error, player_not_found};
        {error, Error} ->
            {error, Error}
    end.

exists(DB, PlayerUUID, Password, GameUUID) ->
    case authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_exists(DB, PlayerUUID, GameUUID);
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_delete(DB, Args) ->
    case database:delete_doc(DB, proplists:get_value("player_uuid", Args)) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            {error, player_not_found};
        {error, Error} ->
            {error, Error}
    end.

do_register(DB, Args) ->
    case database:save_doc(DB, build_doc(Args)) of
        {ok, Doc} ->
            update_password(DB, Doc, proplists:get_value("password", Args));
        {error, Error} ->
            {error, Error}
    end.

do_exists(DB, PlayerUUID, GameUUID) ->
    View = {<<"players">>, <<"by_game">>},
    Keys = {key, views:keys([PlayerUUID, GameUUID])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            {ok, true};
        {error, not_found} ->
            {error, game_not_found};
        {error, Error} ->
            {error, Error}
    end.

update_password(DB, Doc, Password) ->
    PasswordHash = cryptography:sha(Password, document:get_id(Doc)),
    Doc1 = document:set_value("password", PasswordHash, Doc),
    database:save_doc(DB, Doc1).

build_doc(Args) ->
    GameUUID = proplists:lookup("game_uuid", Args),
    PlayerId = proplists:lookup("player_id", Args),
    document:create([GameUUID, PlayerId, ?TYPE]).

%% ###############################################################
%% 
%% ############################################################### 
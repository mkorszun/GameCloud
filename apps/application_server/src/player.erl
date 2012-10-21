%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).

-compile([{parse_transform, lager_transform}]).

-export([create/1, delete/1, authorize/2, authorize_game/3, exists/3]).
-export([create/2, delete/2, authorize/3, authorize_game/4, exists/4]).

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
    GameUUID = proplists:get_value("game_uuid", Args),
    case game:exists(DB, DeveloperId, DevPassword, GameUUID) of
        {ok, true} ->
            do_register(DB, Args);
        {error, Error} ->
            ?ERR("Failed to create player for game id=~p :~p", [GameUUID, Error]),
            {error, Error}
    end.

delete(Args) ->
    delete(application_server_db:connection(), Args).

delete(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    case authorize(DB, PlayerUUID, Password) of
        {ok, true} ->
            do_delete(DB, Args);
        {error, Error} ->
            ?ERR("Failed to delete player ~p: ~p", [PlayerUUID, Error]),
            {error, Error}
    end.

authorize(PlayerUUID, Password) ->
    authorize(application_server_db:connection(), PlayerUUID, Password).

authorize(DB, PlayerUUID, Password) ->
    case database:read_doc(DB, PlayerUUID) of
        {ok, Doc} ->
            authorization:authorize(player, Doc, PlayerUUID, Password);
        {error, not_found} ->
            ?ERR("Failed to find player id=~p", [PlayerUUID]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to find player id=~p: ~p", [PlayerUUID, Error]),
            {error, Error}
    end.

authorize_game(PlayerUUID, Password, GameUUID) ->
    authorize_game(application_server_db:connection(), PlayerUUID, Password, GameUUID).

authorize_game(DB, PlayerUUID, Password, GameUUID) ->
    View = {<<"players">>, <<"by_game">>},
    Keys = {key, views:keys([PlayerUUID, GameUUID])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, [Doc]} ->
            authorization:authorize(player, Doc, PlayerUUID, Password);
        {ok, []} ->
            ?ERR("Failed to find player id=~p for game id=~p", 
                [PlayerUUID, GameUUID]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to find player id=~p for game id=~p: ~p", 
                [PlayerUUID, GameUUID, Error]),
            {error, Error}
    end.

exists(PlayerUUID, Password, GameUUID) ->
    exists(application_server_db:connection(), PlayerUUID, Password, GameUUID).

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
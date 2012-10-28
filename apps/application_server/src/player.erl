%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).

-compile([{parse_transform, lager_transform}]).

-export([create/1, create/2]).
-export([exists1/3, exists1/4, exists2/2, exists2/3]).
-export([authorize1/2, authorize1/3, authorize2/3, authorize2/4]).
-export([delete1/1, delete1/2, delete2/2, delete2/3, delete3/1, delete3/2]).

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
    case game:exists1(DB, DeveloperId, DevPassword, GameUUID) of
        {ok, true} ->
            do_register(DB, Args);
        {error, Error} ->
            ?ERR("Failed to create player for game=~p :~p", 
                [GameUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% DELETE
%% ############################################################### 

delete1(Args) ->
    delete1(application_server_db:connection(), Args).

delete1(DB, Args) ->
    PlayerUUID = proplists:get_value("player_uuid", Args),
    Password = proplists:get_value("password", Args),
    delete2(DB, PlayerUUID, Password).

delete2(PlayerUUID, Password) ->
    delete2(application_server_db:connection(), PlayerUUID, Password).

delete2(DB, PlayerUUID, Password) ->
    case authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            delete3(DB, PlayerUUID);
        {error, Error} ->
            ?ERR("Failed to delete player=~p: ~p", 
                [PlayerUUID, Error]),
            {error, Error}
    end.

delete3(PlayerUUID) ->
    delete3(application_server_db:connection(), PlayerUUID).

delete3(DB, PlayerUUID) ->
    case database:delete_doc(DB, PlayerUUID) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            ?ERR("Failed to find player=~p", 
                [PlayerUUID]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to delete player=~p: ~p", 
                [PlayerUUID, Error]),
            {error, Error}
    end.

%% ###############################################################
%% AUTHORIZE
%% ############################################################### 

authorize1(PlayerUUID, Password) ->
    authorize1(application_server_db:connection(), PlayerUUID, Password).

authorize1(DB, PlayerUUID, Password) ->
    case database:read_doc(DB, PlayerUUID) of
        {ok, Doc} ->
            authorization:authorize(player, Doc, PlayerUUID, Password);
        {error, not_found} ->
            ?ERR("Failed to find player id=~p", 
                [PlayerUUID]),
            {error, player_not_found};
        {error, Error} ->
            ?ERR("Failed to find player id=~p: ~p", 
                [PlayerUUID, Error]),
            {error, Error}
    end.

authorize2(PlayerUUID, Password, SaveUUID) ->
    authorize2(application_server_db:connection(), PlayerUUID, Password, SaveUUID).

authorize2(DB, PlayerUUID, Password, SaveUUID) ->
    View = {<<"players">>, <<"by_game">>},
    Keys = {key, views:keys([PlayerUUID, SaveUUID])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, [Doc]} ->
            authorization:authorize(player, Doc, PlayerUUID, Password);
        {ok, []} ->
            {error, player_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% EXISTS
%% ############################################################### 

exists1(PlayerUUID, Password, GameUUID) ->
    exists1(application_server_db:connection(), PlayerUUID, Password, GameUUID).

exists1(DB, PlayerUUID, Password, GameUUID) ->
    case authorize1(DB, PlayerUUID, Password) of
        {ok, true} ->
            exists2(DB, PlayerUUID, GameUUID);
        {error, Error} ->
            {error, Error}
    end.

exists2(PlayerUUID, GameUUID) ->
    exists2(application_server_db:connection(), PlayerUUID, GameUUID).

exists2(DB, PlayerUUID, GameUUID) ->
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

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_register(DB, Args) ->
    case database:save_doc(DB, build_doc(Args)) of
        {ok, Doc} ->
            update_password(DB, Doc, proplists:get_value("password", Args));
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
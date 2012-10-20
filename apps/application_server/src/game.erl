%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-compile([{parse_transform, lager_transform}]).

-export([create/1, exists/3]).
-export([create/2, exists/4]).

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

exists(DeveloperId, Password, GameUUID) ->
    exists(application_server_db:connection(), DeveloperId, Password, GameUUID).

exists(DB, DeveloperId, Password, GameUUID) ->
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            do_exists(DB, DeveloperId, GameUUID);
        {error, Error} ->
            ?ERR("Failed to find game id=~p for developer=~p: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_exists(DB, DeveloperId, GameUUID) ->
    View = {<<"games">>, <<"by_developer">>},
    Keys = {key, views:keys([DeveloperId, GameUUID])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            {ok, true};
        {error, not_found} ->
            ?ERR("Failed to find game id=~p for developer=~p",
                [GameUUID, DeveloperId]),
            {error, game_not_found};
        {error, Error} ->
            ?ERR("Failed to find game id=~p for developer=~p: ~p",
                [GameUUID, DeveloperId, Error]),
            {error, Error}
    end.

build_doc(Args) ->
    DeveloperId = proplists:lookup("developer_id", Args),
    GameId = proplists:lookup("game_id", Args),
    Description = proplists:lookup("description", Args),
    document:create([DeveloperId, GameId, Description, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
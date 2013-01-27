%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game screen business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_screen).

-export([read/3, read/4]).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

read(DeveloperId, GameKey, ScreenName) ->
    read(application_server_db:connection(), DeveloperId, GameKey, ScreenName).

read(DB, DeveloperId, GameKey, ScreenName) ->
    View = {<<"games">>, <<"read_screen">>},
    Keys = {key, views:keys([DeveloperId, GameKey, ScreenName])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% ###############################################################
%% ###############################################################
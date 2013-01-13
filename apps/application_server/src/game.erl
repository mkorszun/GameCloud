%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-export([create/1, create/2]).
-export([read/2, read/3]).
-export([delete/2, delete/3]).
-export([exists/2, exists/3]).
-export([list_players/2, list_players/3]).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(Game) ->
    create(application_server_db:connection(), Game).

create(DB, Game) ->
    try build_doc(Game) of
        Document -> 
            database:save_doc(DB, Document)
    catch
        _:Error ->
            {error, {bad_data, Error}}
    end.

%% ###############################################################
%% READ
%% ###############################################################

read(DeveloperId, GameKey) ->
    read(application_server_db:connection(), DeveloperId, GameKey).

read(DB, DeveloperId, GameKey) ->
    View = {<<"games">>, <<"read">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% DELETE
%% ###############################################################

delete(DeveloperId, GameKey) ->
    delete(application_server_db:connection(), DeveloperId, GameKey).

delete(DB, DeveloperId, GameKey) ->
    View = {<<"games">>, <<"delete">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    database:delete_doc(DB, View, [Keys]).

%% ###############################################################
%% EXISTS
%% ###############################################################

exists(DeveloperId, GameKey) ->
    exists(application_server_db:connection(), DeveloperId, GameKey).

exists(DB, DeveloperId, GameKey) ->
    View = {<<"games">>, <<"read">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    database:exists(DB, View, [Keys]).

%% ###############################################################
%% LIST PLAYERS
%% ###############################################################

list_players(DeveloperId, GameKey) ->
    list_players(application_server_db:connection(), DeveloperId, GameKey).

list_players(DB, DeveloperId, GameKey) ->
    View = {<<"players">>, <<"all">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

field_mapping(Developer) ->
    [{<<"developer_id">>, {<<"developer_id">>, fun(V) -> V end}}, 
     {<<"name">>, {<<"name">>, fun(V) -> V end}},
     {<<"description">>, {<<"description">>, fun(V) -> V end}},
     {<<"platform">>, {<<"platform">>, fun(V) -> V end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) -> V end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) -> V end}}].

build_doc(Developer) ->
    Mapping = field_mapping(Developer),
    Doc = document:build_doc(Developer, [], Mapping),
    document:create([{<<"type">>, <<"game">>} | Doc]).

%% ###############################################################
%% 
%% ###############################################################
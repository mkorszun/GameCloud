%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-export([create/1, create/2]).
-export([read/2, read/3]).
-export([update/3, update/4]).
-export([delete/2, delete/3]).
-export([exists/2, exists/3]).

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
%% UPDATE
%% ###############################################################

update(DeveloperId, GameKey, NewData) ->
    update(application_server_db:connection(), DeveloperId, GameKey, NewData).

update(DB, DeveloperId, GameKey, NewData) ->
    View = {<<"games">>, <<"update">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, Doc} ->
            try update_doc(Doc, NewData) of
                NewDoc ->
                    database:save_doc(DB, NewDoc)
            catch
                _:Error ->
                    {error, {bad_data, Error}}
            end;
        {error, Error} ->
            {error, Error}
    end.

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
%% INTERNAL FUNCTIONS
%% ###############################################################

field_mapping(create, _Game) ->
    [{<<"developer_id">>, {<<"developer_id">>, fun(V) -> V end}},
     {<<"name">>, {<<"name">>, fun(V) -> V end}},
     {<<"description">>, {<<"description">>, fun(V) -> V end}},
     {<<"platform">>, {<<"platform">>, fun(V) -> V end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) -> V end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) -> V end}}];

field_mapping(update, _Game) ->
    [{<<"name">>, {<<"name">>, fun(V) -> V end}},
     {<<"description">>, {<<"description">>, fun(V) -> V end}},
     {<<"platform">>, {<<"platform">>, fun(V) -> V end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) -> V end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) -> V end}}].

build_doc(Game) ->
    Mapping = field_mapping(create, Game),
    Doc = document:build_doc(Game, [], Mapping),
    document:create([{<<"type">>, <<"game">>} | Doc]).

update_doc(Game, Fields) ->
    Mapping = field_mapping(update, Fields),
    document:update_doc(Game, Fields, Mapping).

%% ###############################################################
%%
%% ###############################################################
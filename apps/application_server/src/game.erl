%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).

-export([create/1, create/2]).
-export([read/2, read/3]).
-export([read_screen/3, read_screen/4]).
-export([update/3, update/4]).
-export([delete/2, delete/3]).
-export([path/2]).

-export([schema/0]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("data_model.hrl").

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(Game) ->
    create(application_server_db:connection(), Game).

create(DB, Game) ->
    try check(?GAME_SCHEMA, Game) of
        {ok, Game} ->
            database:save_doc(DB, build_doc(Game))
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

read_screen(DeveloperId, GameKey, ScreenName) ->
    read_screen(application_server_db:connection(), DeveloperId, GameKey, ScreenName).

read_screen(DB, DeveloperId, GameKey, ScreenName) ->
    View = {<<"games">>, <<"read_screen">>},
    Keys = {key, views:keys([DeveloperId, GameKey, ScreenName])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% UPDATE
%% ###############################################################

update(DeveloperId, GameKey, NewData) ->
    update(application_server_db:connection(), DeveloperId, GameKey, NewData).

update(DB, DeveloperId, GameKey, NewData) ->
    try check(?GAME_SCHEMA, NewData) of
        {ok, NewData} ->
            View = {<<"games">>, <<"update">>},
            Keys = {key, views:keys([DeveloperId, GameKey])},
            case database:read_doc(DB, View, [Keys]) of
                {ok, Doc} ->
                    database:save_doc(DB, update_doc(Doc, NewData));
                {error, Error} ->
                    {error, Error}
            end
    catch
        _:Error ->
            {error, {bad_data, Error}}
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
%% RESOURCE PATH BUILDER
%% ###############################################################

path(game, Game) ->
    DeveloperId = document:read(<<"developer_id">>, Game),
    GameKey = document:read(<<"key">>, Game),
    lists:flatten(io_lib:format("/developer/~s/game/~s", [DeveloperId, GameKey]));

path(screen, Game) ->
    Screen = document:read(<<"screen">>, Game),
    ScreenName = document:read(<<"name">>, Screen),
    lists:flatten(io_lib:format("~s/screen/~s", [path(game, Game), ScreenName])).

%% ###############################################################
%% SCHEMA
%% ###############################################################

schema() ->
    io:format("~s", [mochijson2:encode(?GAME_SCHEMA)]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Game) ->
    Trans = transform(create, Game),
    document:build_doc(mochijson2, Trans).

update_doc(OldGame, NewGame) ->
    Trans = transform(update, NewGame),
    Doc = struct:extend(document:to_json(OldGame), Trans),
    document:build_doc(mochijson2, Doc).

transform(_, Data) ->
    Fields = fields(create, Data),
    struct:set_value(Fields, Data).

fields(_, Data) ->
    [{<<"type">>, <<"game">>}].

%% ###############################################################
%% ###############################################################
%% ###############################################################
%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer).

-export([create/1, create/2]).
-export([read/1, read/2]).
-export([update/2, update/3]).
-export([delete/1, delete/2]).
-export([authorize/2, authorize/3]).
-export([list_games/2, list_games/3]).

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

create(Developer) ->
    create(application_server_db:connection(), Developer).

create(DB, Developer) ->
    try check(?DEVELOPER_SCHEMA, Developer) of
        {ok, Developer} ->
            database:save_doc(DB, build_doc(Developer))
    catch
        _:Error ->
            {error, {bad_data, Error}}
    end.

%% ###############################################################
%% READ
%% ###############################################################

read(Id) ->
    read(application_server_db:connection(), Id).

read(DB, Id) ->
    View = {<<"developers">>, <<"read">>},
    Keys = {key, views:keys([Id])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% UPDATE
%% ###############################################################

update(Id, NewData) ->
    update(application_server_db:connection(), Id, NewData).

update(DB, Id, NewData) ->
    try check(?DEVELOPER_SCHEMA, NewData) of
        {ok, NewData} ->
            case database:read_doc(DB, Id) of
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

delete(Id) ->
    delete(application_server_db:connection(), Id).

delete(DB, Id) ->
    database:delete_doc(DB, Id).

%% ###############################################################
%% AUTHORIZE
%% ###############################################################

authorize(Id, Password) ->
    authorize(application_server_db:connection(), Id, Password).

authorize(DB, Id, Password) ->
    case database:read_doc(DB, Id) of
        {ok, Doc} ->
            authorization:authorize(Doc, Id, Password);
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% LIST GAMES
%% ###############################################################

list_games(Id, Exclude) ->
    list_games(application_server_db:connection(), Id, Exclude).

list_games(DB, Id, Exclude) ->
    View = {<<"games">>, <<"list">>},
    Keys = {key, views:keys([Id])},
    case database:read_doc(DB, View, [Keys], false) of
        {ok, Games} ->
            {ok, document:exclude(Games, Exclude, <<"name">>)};
        {error, Reason} ->
            {error, Reason}
    end.

%% ###############################################################
%% SCHEMA
%% ###############################################################

schema() ->
    io:format("~s", [mochijson2:encode(?DEVELOPER_SCHEMA)]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Developer) ->
    Trans = transform(create, Developer),
    document:build_doc(mochijson2, Trans).

update_doc(OldDeveloper, NewDeveloper) ->
    Id = document:get_id(OldDeveloper),
    Trans = transform(update, struct:set_value(<<"id">>, Id, NewDeveloper)),
    Doc = struct:extend(document:to_json(OldDeveloper), Trans),
    document:build_doc(mochijson2, Doc).

transform(_, Data) ->
    Fields = fields(create, Data),
    struct:set_value(Fields, Data).

fields(create, Data) ->
    [{<<"type">>, <<"developer">>},
     {<<"_id">>, struct:get_value(<<"id">>, Data)},
     {<<"password">>, authorization:sha(struct:get_value(<<"password">>, Data),
                                        struct:get_value(<<"id">>, Data))}];
fields(update, Data) ->
    [{<<"type">>, <<"developer">>},
     {<<"password">>, authorization:sha(struct:get_value(<<"password">>, Data),
                                        struct:get_value(<<"id">>, Data))}].

%% ###############################################################
%% ###############################################################
%% ###############################################################
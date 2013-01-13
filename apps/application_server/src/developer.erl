%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer).

-export([create/1, create/2]).
-export([read/1, read/2]).
-export([delete/1, delete/2]).
-export([authorize/2, authorize/3]).
-export([list_games/1, list_games/2]).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(Developer) ->
    create(application_server_db:connection(), Developer).

create(DB, Developer) ->
    try build_doc(Developer) of
        Document -> 
            database:save_doc(DB, Document)
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

list_games(Id) ->
    list_games(application_server_db:connection(), Id).

list_games(DB, Id) ->
    View = {<<"games">>, <<"list">>},
    Keys = {key, views:keys([Id])},
    database:read_doc(DB, View, [Keys], false).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

field_mapping(Developer) ->
    [{<<"id">>, {<<"_id">>, fun(V) -> V end}}, 
     {<<"email">>, {<<"email">>, fun(V) -> V end}},
     {<<"password">>, {<<"password">>, fun(V) -> Id = 
        proplists:get_value(<<"id">>, Developer), 
        authorization:sha(V, Id) end}}].

build_doc(Developer) ->
    Mapping = field_mapping(Developer),
    Doc = document:build_doc(Developer, [], Mapping),
    document:create([{<<"type">>, <<"developer">>} | Doc]).

%% ###############################################################
%%
%% ###############################################################
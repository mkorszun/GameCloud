%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).

-export([create/1, create/2]).
-export([read/1, read/2]).
-export([delete/1, delete/2]).
-export([authorize/3, authorize/4]).

%% ###############################################################
%% API
%% ############################################################### 

%% ###############################################################
%% CREATE
%% ############################################################### 

create(Args) ->
    create(application_server_db:connection(), Args).

create(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    GameKey = proplists:get_value("game_key", Args),
    create(DB, Args, game:exists(DB, DeveloperId, GameKey)).

%% ###############################################################
%% READ
%% ############################################################### 

read(Id) ->
    read(application_server_db:connection(), Id).

read(DB, Id) ->
    database:read_doc(DB, Id).

%% ###############################################################
%% DELETE
%% ############################################################### 

delete(Id) ->
    delete(application_server_db:connection(), Id).

delete(DB, Id) ->
    database:delete(DB, Id).

%% ###############################################################
%% AUTHORIZE
%% ############################################################### 

authorize(GameKey, PlayerId, Password) ->
    authorize(application_server_db:connection(), GameKey, PlayerId, Password).

authorize(DB, GameKey, PlayerId, Password) ->
    View = {<<"players">>, <<"authorize">>},
    Keys = {key, views:keys([GameKey, PlayerId])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, Doc} ->
            authorization:authorize(Doc, PlayerId, Password);
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

create(DB, Player, true) ->
    database:save_doc(DB, build_doc(Player));
create(_, _, false) ->
    {error, not_found};
create(_, _, Else) ->
    Else.

field_mapping(Developer) ->
    [{<<"developer_id">>, {<<"developer_id">>, fun(V) -> V end}}, 
     {<<"game_key">>, {<<"game_key">>, fun(V) -> V end}},
     {<<"player_id">>, {<<"_id">>, fun(V) -> Id = V end}},
     {<<"password">>, <<"password">>}].

build_doc(Developer) ->
    Mapping = field_mapping(Developer),
    Doc = document:build_doc(Developer, [], Mapping),
    document:create([{<<"type">>, <<"player">>} | Doc]).

%% ###############################################################
%% 
%% ############################################################### 
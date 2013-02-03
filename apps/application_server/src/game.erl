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

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(Game) ->
    create(application_server_db:connection(), Game).

create(_, []) ->
    {error, {bad_data, empty_doc}};
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

update(_, _, _, []) ->
    {error, {bad_data, empty_doc}};
update(DB, DeveloperId, GameKey, NewData) ->
    View = {<<"games">>, <<"update">>},
    Keys = {key, views:keys([DeveloperId, GameKey])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, Doc} ->
            try update_doc(Doc, NewData) of
                NewDoc when NewDoc =:= Doc  ->
                    {ok, Doc};
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
%% INTERNAL FUNCTIONS
%% ###############################################################

field_mapping(create, Game) ->
    [{<<"developer_id">>, {<<"developer_id">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}} | field_mapping(Game)];

field_mapping(update, Game) ->
    field_mapping(Game).

field_mapping(_Game) ->
    [{<<"name">>, {<<"name">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}},
     {<<"description">>, {<<"description">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}},
     {<<"platform">>, {<<"platform">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) when is_binary(V) -> V;
        (_) -> throw(bad_format) end}},
     {<<"screen">>, {<<"screen">>, fun({struct, V}) when is_list(V) ->
            {[{<<"name">>, case proplists:get_value(<<"name">>, V) of
                undefined -> undefined;
                E when is_binary(E) -> E;
                _ -> throw(bad_format) end},
            {<<"content_type">>, case proplists:get_value(<<"content_type">>, V) of
                undefined -> undefined;
                E when is_binary(E) -> E;
                _ -> throw(bad_format) end},
            {<<"content">>, case proplists:get_value(<<"content">>, V) of
                E when is_binary(E) -> E;
                _ -> throw(bad_format) end}]
            };
        (_) -> throw(bad_format) end}}].

build_doc(Game) ->
    Mapping = field_mapping(create, Game),
    Doc = document:create(Game, [], Mapping),
    {[{<<"type">>, <<"game">>} | Doc]}.

update_doc(Game, Fields) ->
    Mapping = field_mapping(update, Fields),
    document:update(Game, Fields, Mapping).

%% ###############################################################
%% ###############################################################
%% ###############################################################
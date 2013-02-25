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
%% INTERNAL FUNCTIONS
%% ###############################################################

field_mapping(create, Game) ->
    [{<<"developer_id">>, {<<"developer_id">>, fun(V) -> check(developer_id, V, Game) end}},
     {<<"name">>, {<<"name">>, fun(V) -> check(name, V, Game) end}},
     {<<"description">>, {<<"description">>, fun(V) -> check(description, V, Game) end}},
     {<<"platform">>, {<<"platform">>, fun(V) -> check(platform, V, Game) end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) -> check(game_link, V, Game) end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) -> check(market_link, V, Game) end}},
     {<<"tags">>, {<<"tags">>, fun(V) -> check(tags, V, Game) end}},
     {<<"status">>, {<<"status">>, fun(V) -> check(status, V, Game) end}},
     {<<"screen">>, {<<"screen">>, fun(V) -> check(screen, V, Game) end}}];

field_mapping(update, Game) ->
    [{<<"name">>, {<<"name">>, fun(V) -> check(name, V, Game) end}},
     {<<"description">>, {<<"description">>, fun(V) -> check(description, V, Game) end}},
     {<<"platform">>, {<<"platform">>, fun(V) -> check(platform, V, Game) end}},
     {<<"game_link">>, {<<"game_link">>, fun(V) -> check(game_link, V, Game) end}},
     {<<"market_link">>, {<<"market_link">>, fun(V) -> check(market_link, V, Game) end}},
     {<<"tags">>, {<<"tags">>, fun(V) -> check(tags, V, Game) end}},
     {<<"status">>, {<<"status">>, fun(V) -> check(status, V, Game) end}},
     {<<"screen">>, {<<"screen">>, fun(V) -> check(screen, V, Game) end}}].

build_doc(Game) ->
    Mapping = field_mapping(create, Game),
    Doc = document:create(Game, [], Mapping),
    {[{<<"type">>, <<"game">>} | Doc]}.

update_doc(Game, Fields) ->
    Mapping = field_mapping(update, Fields),
    document:update(Game, Fields, Mapping).

%% ###############################################################
%% FIELD VALIDATION
%% ###############################################################

check(developer_id, V, _) when is_binary(V) -> V;
check(developer_id, _, _) -> throw(wrong_developer_id_format);

check(name, V, _) when is_binary(V) -> V;
check(name, _, _) -> throw(wrong_name_format);

check(description, V, _) when is_binary(V) -> V;
check(description, _, _) -> throw(wrong_description_format);

check(platform, <<"android">> = V, _) -> V;
check(platform, <<"ios">> = V, _) -> V;
check(platform, _, _) -> throw(wrong_platform_format);

check(game_link, V, _) when is_binary(V) -> V;
check(game_link, _, _) -> throw(wrong_game_link_format);

check(market_link, V, _) when is_binary(V) -> V;
check(market_link, _, _) -> throw(wrong_market_link_format);

check(tags, [H|_] = V, _) when is_list(V), is_binary(H) -> V;
check(tags, _, _) -> throw(wrong_tags_format);

check(status, <<"new">> = V, _) -> V;
check(status, <<"beta">> = V, _) -> V;
check(status, <<"published">> = V, _) -> V;
check(status, _, _) -> throw(wrong_status_format);

check(screen, {struct, V}, Game) when is_list(V) ->
    {[{<<"name">>, check([screen, name], V, Game)},
      {<<"content_type">>, check([screen, content_type], V, Game)},
      {<<"content">>, check([screen, content], V, Game)}]};
check(screen, _, _) -> throw(wrong_screen_format);

check([screen, name], V, _) ->
    case proplists:get_value(<<"name">>, V) of
        E when is_binary(E) -> E;
        undefined -> throw(missing_screen_name);
        _ -> throw(wrong_screen_name_format)
    end;

check([screen, content_type], V, _) ->
    case proplists:get_value(<<"content_type">>, V) of
        E when is_binary(E) -> E;
        undefined -> throw(missing_screen_content_type);
        _ -> throw(wrong_screen_content_type_format)
    end;

check([screen, content], V, _) ->
    case proplists:get_value(<<"content">>, V) of
        E when is_binary(E) -> E;
        undefined -> throw(missing_screen_content);
        _ -> throw(wrong_screen_content_format)
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
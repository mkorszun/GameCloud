%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game tests suite
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% ###############################################################
%% CT API
%% ###############################################################

all() -> [
    {group, create},
    {group, read},
    {group, read_screen},
    {group, update},
    {group, delete},
    {group, path}
].

groups() -> [
    {create, [], create()},
    {read, [], read()},
    {read_screen, [], read_screen()},
    {update, [], update()},
    {delete, [], delete()},
    {path, [], path()}
].

%% ###############################################################
%%
%% ###############################################################

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(Config) ->
    application:stop(meck),
    Config.

%% ###############################################################
%% TESTS
%% ###############################################################

create() -> [
    test_game_create,
    test_game_create_additional_elements,
    test_game_create_empty_doc,
    test_game_create_missing_element,
    test_game_create_wrong_format
].

read() -> [
    test_game_read
].

read_screen() -> [
    test_game_read_screen
].

update() -> [
    test_game_update,
    test_game_update_no_change,
    test_game_update_wrong_format,
    test_game_update_doc_not_found,
    test_game_update_empty_doc
].

delete() -> [
    test_game_delete
].

path() -> [
    test_path_game,
    test_path_screen
].

%% ###############################################################
%% TEST HELPERS
%% ###############################################################

-define(CREATE_DATA,
    [
        {<<"developer_id">>, <<"id1">>},
        {<<"name">>, <<"wc3">>},
        {<<"description">>, <<"baam">>},
        {<<"platform">>, <<"ios">>},
        {<<"game_link">>, <<"g_link">>},
        {<<"market_link">>, <<"m_link">>},
        {<<"tags">>, [<<"one">>, <<"two">>]},
        {<<"status">>, <<"new">>},
        {<<"screen">>, {struct, [
            {<<"name">>, <<"screen.jpg">>},
            {<<"content_type">>, <<"image/jpg">>},
            {<<"content">>, <<"kjkasdjksadjsa">>}
        ]}}
    ]).

-define(DOC,
    {[
        {<<"type">>, <<"game">>},
        {<<"screen">>, {[
            {<<"name">>, <<"screen.jpg">>},
            {<<"content_type">>, <<"image/jpg">>},
            {<<"content">>, <<"kjkasdjksadjsa">>}]}},
        {<<"status">>, <<"new">>},
        {<<"tags">>, [<<"one">>, <<"two">>]},
        {<<"market_link">>, <<"m_link">>},
        {<<"game_link">>, <<"g_link">>},
        {<<"platform">>, <<"ios">>},
        {<<"description">>, <<"baam">>},
        {<<"name">>, <<"wc3">>},
        {<<"developer_id">>, <<"id1">>}
    ]}).

-define(UPDATED_DOC,
    {[
        {<<"type">>, <<"game">>},
        {<<"screen">>, {[
            {<<"name">>, <<"screen.jpg_updated">>},
            {<<"content_type">>, <<"image/jpg_updated">>},
            {<<"content">>, <<"kjkasdjksadjsa_updated">>}]}},
        {<<"status">>, <<"new">>},
        {<<"tags">>, [<<"one">>, <<"two">>]},
        {<<"market_link">>, <<"m_link">>},
        {<<"game_link">>, <<"g_link_updated">>},
        {<<"platform">>, <<"ios">>},
        {<<"description">>, <<"baam">>},
        {<<"name">>, <<"wc3_updated">>},
        {<<"developer_id">>, <<"id1">>}
    ]}).

%% ###############################################################
%% CREATE
%% ###############################################################

test_game_create(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = game:create([], ?CREATE_DATA),
    [{_,{database, save_doc, [[], ?DOC]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_create_additional_elements(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = game:create([], [{<<"a">>,<<"b">>} | ?CREATE_DATA]),
    [{_,{database, save_doc, [[], ?DOC]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_create_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} = game:create([], []).

test_game_create_missing_element(_Config) ->
    Id = {<<"developer_id">>, <<"a">>},
    Name = {<<"name">>, <<"b">>},
    Description = {<<"description">>, <<"c">>},
    Platform = {<<"platform">>, <<"ios">>},
    GameLink = {<<"game_link">>, <<"d">>},
    MarketLink = {<<"market_link">>, <<"e">>},
    Tags = {<<"tags">>, [<<"f">>]},
    Status = {<<"status">>, <<"new">>},
    ScreenName = {<<"name">>, <<"g">>},
    ScreenContentType = {<<"content_type">>, <<"h">>},
    {error, {bad_data, missing_developer_id}} = game:create([],[Name]),
    {error, {bad_data, missing_name}} = game:create([],[Id]),
    {error, {bad_data, missing_description}} = game:create([],[Id, Name]),
    {error, {bad_data, missing_platform}} = game:create([],[Id, Name, Description]),
    {error, {bad_data, missing_game_link}} = game:create([],[Id, Name, Description, Platform]),
    {error, {bad_data, missing_market_link}} = game:create([],[Id, Name, Description, Platform, GameLink]),
    {error, {bad_data, missing_tags}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink]),
    {error, {bad_data, missing_status}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink, Tags]),
    {error, {bad_data, missing_screen}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink, Tags, Status]),
    {error, {bad_data, missing_screen_name}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink, Tags, Status, {<<"screen">>, {struct, [ScreenContentType]}}]),
    {error, {bad_data, missing_screen_content_type}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink, Tags, Status, {<<"screen">>, {struct, [ScreenName]}}]),
    {error, {bad_data, missing_screen_content}} = game:create([],[Id, Name, Description, Platform, GameLink, MarketLink, Tags, Status, {<<"screen">>, {struct, [ScreenName, ScreenContentType]}}]).

test_game_create_wrong_format(_Config) ->
    Id = fun(ok) -> {<<"developer_id">>, <<"a">>}; (error) -> {<<"developer_id">>, a} end,
    Name = fun(ok) -> {<<"name">>, <<"b">>}; (error) -> {<<"name">>, b} end,
    Description = fun(ok) -> {<<"description">>, <<"c">>}; (error) -> {<<"description">>, c} end,
    Platform = fun(ok) -> {<<"platform">>, <<"ios">>}; (error) -> {<<"platform">>, ios} end,
    GameLink = fun(ok) -> {<<"game_link">>, <<"d">>}; (error) -> {<<"game_link">>, d} end,
    MarketLink = fun(ok) -> {<<"market_link">>, <<"e">>}; (error) -> {<<"market_link">>, e} end,
    Tags = fun(ok) -> {<<"tags">>, [<<"f">>]}; (error) -> {<<"tags">>, [f]} end,
    Status = fun(ok) -> {<<"status">>, <<"new">>}; (error) -> {<<"status">>, new} end,
    ScreenName = fun(ok) -> {<<"name">>, <<"g">>}; (error) -> {<<"name">>, g} end,
    ScreenContentType = fun(ok) -> {<<"content_type">>, <<"h">>}; (error) -> {<<"content_type">>, h} end,
    ScreenContent = fun(ok) -> {<<"content">>, <<"i">>}; (error) -> {<<"content">>, i} end,
    {error, {bad_data, wrong_developer_id_format}} = game:create([],[Id(error)]),
    {error, {bad_data, wrong_name_format}} = game:create([],[Id(ok), Name(error)]),
    {error, {bad_data, wrong_description_format}} = game:create([],[Id(ok), Name(ok), Description(error)]),
    {error, {bad_data, wrong_platform_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(error)]),
    {error, {bad_data, wrong_game_link_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(error)]),
    {error, {bad_data, wrong_market_link_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(error)]),
    {error, {bad_data, wrong_tags_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(error)]),
    {error, {bad_data, wrong_status_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(error)]),
    {error, {bad_data, wrong_screen_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, []}]),
    {error, {bad_data, wrong_screen_name_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(error)]}}]),
    {error, {bad_data, wrong_screen_content_type_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(ok), ScreenContentType(error)]}}]),
    {error, {bad_data, wrong_screen_content_format}} = game:create([],[Id(ok), Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(ok), ScreenContentType(ok), ScreenContent(error)]}}]).

%% ###############################################################
%% READ
%% ###############################################################

test_game_read(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = game:read([], <<"id">>, <<"game_key">>),
    [{_, {database, read_doc, [[], {<<"games">>, <<"read">>}, [{key, [<<"id">>, <<"game_key">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% READ SCREEN
%% ###############################################################

test_game_read_screen(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = game:read_screen([], <<"id">>, <<"game_key">>, <<"screen_name">>),
    [{_, {database, read_doc, [[], {<<"games">>, <<"read_screen">>}, [{key, [<<"id">>, <<"game_key">>, <<"screen_name">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% UPDATE
%% ###############################################################

test_game_update(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = game:update([], <<"id">>, <<"key">>, [
        {<<"name">>, <<"wc3_updated">>},
        {<<"game_link">>, <<"g_link_updated">>},
        {<<"screen">>, {struct, [
            {<<"name">>, <<"screen.jpg_updated">>},
            {<<"content_type">>, <<"image/jpg_updated">>},
            {<<"content">>, <<"kjkasdjksadjsa_updated">>}]}}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {ok, ?DOC}},
     {_, {database, save_doc, [[], ?UPDATED_DOC]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_no_change(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    {ok, ?DOC} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, <<"wc3">>}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_wrong_format(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    Name = fun(ok) -> {<<"name">>, <<"b">>}; (error) -> {<<"name">>, b} end,
    Description = fun(ok) -> {<<"description">>, <<"c">>}; (error) -> {<<"description">>, c} end,
    Platform = fun(ok) -> {<<"platform">>, <<"ios">>}; (error) -> {<<"platform">>, ios} end,
    GameLink = fun(ok) -> {<<"game_link">>, <<"d">>}; (error) -> {<<"game_link">>, d} end,
    MarketLink = fun(ok) -> {<<"market_link">>, <<"e">>}; (error) -> {<<"market_link">>, e} end,
    Tags = fun(ok) -> {<<"tags">>, [<<"f">>]}; (error) -> {<<"tags">>, [f]} end,
    Status = fun(ok) -> {<<"status">>, <<"new">>}; (error) -> {<<"status">>, new} end,
    ScreenName = fun(ok) -> {<<"name">>, <<"g">>}; (error) -> {<<"name">>, g} end,
    ScreenContentType = fun(ok) -> {<<"content_type">>, <<"h">>}; (error) -> {<<"content_type">>, h} end,
    ScreenContent = fun(ok) -> {<<"content">>, <<"i">>}; (error) -> {<<"content">>, i} end,
    {error, {bad_data, wrong_name_format}} = game:update([], <<"id">>, <<"key">>, [Name(error)]),
    {error, {bad_data, wrong_description_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(error)]),
    {error, {bad_data, wrong_platform_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(error)]),
    {error, {bad_data, wrong_game_link_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(error)]),
    {error, {bad_data, wrong_market_link_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(error)]),
    {error, {bad_data, wrong_tags_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(error)]),
    {error, {bad_data, wrong_status_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(error)]),
    {error, {bad_data, wrong_screen_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, []}]),
    {error, {bad_data, wrong_screen_name_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(error)]}}]),
    {error, {bad_data, wrong_screen_content_type_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(ok), ScreenContentType(error)]}}]),
    {error, {bad_data, wrong_screen_content_format}} = game:update([], <<"id">>, <<"key">>, [Name(ok), Description(ok), Platform(ok), GameLink(ok), MarketLink(ok), Tags(ok), Status(ok), {<<"screen">>, {struct, [ScreenName(ok), ScreenContentType(ok), ScreenContent(error)]}}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {ok, ?DOC}} | _] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_doc_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {error, not_found} end),
    {error, not_found} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, <<"wc4">>}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {error, not_found}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} = game:update([], <<"id">>, <<"key">>, []).

%% ###############################################################
%% DELETE
%% ###############################################################

test_game_delete(_Config) ->
    meck:new(database),
    meck:expect(database, delete_doc, fun(_,_,_) -> {ok, doc} end),
    {ok, doc} = game:delete([], <<"id">>, <<"key">>),
    [{_, {database, delete_doc, [[], {<<"games">>, <<"delete">>}, [{key, [<<"id">>, <<"key">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% BUILD PATH
%% ###############################################################

test_path_game(_Config) ->
    "/developer/id1/game/12345" = game:path(game, document:set_value(<<"key">>, <<"12345">>, ?DOC)).

test_path_screen(_Config) ->
    "/developer/id1/game/12345/screen/screen.jpg" = game:path(screen, document:set_value(<<"key">>, <<"12345">>, ?DOC)).

%% ###############################################################
%% ###############################################################
%% ###############################################################
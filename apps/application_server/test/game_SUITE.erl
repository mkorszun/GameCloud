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
    {group, delete}
].

groups() -> [
    {create, [], create()},
    {read, [], read()},
    {read_screen, [], read_screen()},
    {update, [], update()},
    {delete, [], delete()}
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
    test_game_create_missing_element,
    test_game_create_empty_doc,
    test_game_create_bad_format
].

read() -> [
    test_game_read
].

read_screen() -> [
    test_game_read_screen
].

update() -> [
    test_game_update,
    test_game_update_no_change1,
    test_game_update_no_change2,
    test_game_update_bad_format,
    test_game_update_doc_not_found,
    test_game_update_empty_doc
].

delete() -> [
    test_game_delete
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

test_game_create_missing_element(_Config) ->
    {error, {bad_data, missing_element}} =
        game:create([],[
            {<<"developer_id">>, <<"id">>},
            {<<"platform">>, <<"ios">>}]).

test_game_create_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} =
        game:create([], []).

test_game_create_bad_format(_Config) ->
    {error, {bad_data, bad_format}} =
        game:create([], [{<<"developer_id">>, {<<"a">>, <<"b">>}}]).

%% ###############################################################
%% READ
%% ###############################################################

test_game_read(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = game:read([], <<"id">>, <<"game_key">>),
    [{_, {database, read_doc, [[], {<<"games">>, <<"read">>},
        [{key, [<<"id">>, <<"game_key">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% READ SCREEN
%% ###############################################################

test_game_read_screen(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = game:read_screen([], <<"id">>, <<"game_key">>, <<"screen_name">>),
    [{_, {database, read_doc, [[], {<<"games">>, <<"read_screen">>},
        [{key, [<<"id">>, <<"game_key">>, <<"screen_name">>]}]]}, {ok, doc}}] = meck:history(database),
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

test_game_update_no_change1(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    {ok, ?DOC} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, <<"wc3">>}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]},
        {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_no_change2(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    {ok, ?DOC} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, <<"wc3">>}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]},
        {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_bad_format(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {ok, ?DOC} end),
    {error, {bad_data, bad_format}} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, {a,b}}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]},
        {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_doc_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {error, not_found} end),
    {error, not_found} = game:update([], <<"id">>, <<"key">>, [{<<"name">>, <<"wc4">>}]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]},
        {error, not_found}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} = game:update([], <<"id">>, <<"key">>, []).

%% ###############################################################
%% DELETE
%% ###############################################################

test_game_delete(_Config) ->
    meck:new(database),
    meck:expect(database, delete_doc, fun(_,_,_
        ) -> {ok, doc} end),
    {ok, doc} = game:delete([], <<"id">>, <<"key">>),
    [{_, {database, delete_doc, [[], {<<"games">>, <<"delete">>},
        [{key, [<<"id">>, <<"key">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% ###############################################################
%% ###############################################################
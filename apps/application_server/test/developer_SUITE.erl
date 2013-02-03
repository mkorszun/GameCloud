%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer tests suite
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% ###############################################################
%% CT API
%% ###############################################################

all() -> [
    {group, create},
    {group, read},
    {group, update},
    {group, delete},
    {group, authorize},
    {group, list_games}
].

groups() -> [
    {create, [], create()},
    {read, [], read()},
    {update, [], update()},
    {delete, [], delete()},
    {authorize, [], authorize()},
    {list_games, [], list_games()}
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
    test_developer_create,
    test_developer_create_additional_elements,
    test_developer_create_missing_element,
    test_developer_create_empty_doc,
    test_developer_create_bad_format
].

read() -> [
    test_developer_read
].

update() -> [
    test_developer_update,
    test_developer_update_no_change1,
    test_developer_update_no_change2,
    test_developer_update_bad_format,
    test_developer_update_doc_not_found,
    test_developer_update_empty_doc
].

delete() -> [
    test_developer_delete
].

authorize() -> [
    test_developer_authorize_true,
    test_developer_authorize_false,
    test_developer_authorize_not_found
].

list_games() -> [
    test_developer_list_games
].

%% ###############################################################
%% TEST HELPERS
%% ###############################################################

-define(CREATE_DATA,
    [
        {<<"id">>, <<"id1">>},
        {<<"email">>, <<"a@b">>},
        {<<"password">>, <<"baam">>}
    ]).

-define(DOC,
    {[
        {<<"_id">>, <<"id1">>},
        {<<"email">>, <<"a@b">>},
        {<<"password">>, <<"5886A61A13CB927813286ECDCEF509FD7A04535F">>}
    ]}).

-define(UPDATED_DOC,
    {[
        {<<"_id">>, <<"id1">>},
        {<<"email">>, <<"a@b">>},
        {<<"password">>, <<"A340A2AC8CC8A7E2E7902F588FDF622575BB1937">>}
    ]}).

%% ###############################################################
%% CREATE
%% ###############################################################

test_developer_create(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = developer:create([], ?CREATE_DATA),
    ExpDoc = {[
        {<<"type">>, <<"developer">>},
        {<<"password">>, <<"5886A61A13CB927813286ECDCEF509FD7A04535F">>},
        {<<"email">>, <<"a@b">>},
        {<<"_id">>, <<"id1">>}
    ]},
    [{_,{database, save_doc, [[], ExpDoc]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_create_additional_elements(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = developer:create([], [{<<"a">>,<<"b">>} | ?CREATE_DATA]),
    ExpDoc = {[
        {<<"type">>, <<"developer">>},
        {<<"password">>, <<"5886A61A13CB927813286ECDCEF509FD7A04535F">>},
        {<<"email">>, <<"a@b">>},
        {<<"_id">>, <<"id1">>}
    ]},
    [{_,{database, save_doc, [[], ExpDoc]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_create_missing_element(_Config) ->
    {error, {bad_data, missing_element}} =
        developer:create([],[
            {<<"email">>, <<"a@b">>},
            {<<"password">>, <<"baam">>}]).

test_developer_create_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} =
        developer:create([], []).

test_developer_create_bad_format(_Config) ->
    {error, {bad_data, bad_format}} =
        developer:create([], [
            {<<"id">>, <<"id1">>},
            {<<"email">>, {<<"a@b">>, <<"sad">>}},
            {<<"password">>, <<"baam">>}]).

%% ###############################################################
%% READ
%% ###############################################################

test_developer_read(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = developer:read([], <<"id">>),
    [{_, {database, read_doc, [[], {<<"developers">>, <<"read">>},
        [{key, [<<"id">>]}]]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% UPDATE
%% ###############################################################

test_developer_update(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = developer:update([], <<"id">>, [{<<"password">>, <<"pass">>}]),
    [{_, {database, read_doc, [[], <<"id">>]}, {ok, ?DOC}},
     {_, {database, save_doc, [[], ?UPDATED_DOC]}, {ok,doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_update_no_change1(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    {ok, ?DOC} = developer:update([], <<"id">>, [{<<"email">>, <<"a@b">>}]),
    [{_, {database, read_doc, [[], <<"id">>]}, {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_update_no_change2(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    {ok, ?DOC} = developer:update([], <<"id">>, [{<<"a">>, <<"b">>}]),
    [{_, {database, read_doc, [[], <<"id">>]}, {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_update_bad_format(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    {error, {bad_data, bad_format}} = developer:update([], <<"id">>, [{<<"email">>, {a,b}}]),
    [{_, {database, read_doc, [[], <<"id">>]}, {ok, ?DOC}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_update_doc_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {error, not_found} end),
    {error, not_found} = developer:update([], <<"id">>, [{<<"email">>, <<"a@b">>}]),
    [{_, {database, read_doc, [[], <<"id">>]}, {error, not_found}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_update_empty_doc(_Config) ->
    {error, {bad_data, empty_doc}} = developer:update([], <<"id">>, []).

%% ###############################################################
%% DELETE
%% ###############################################################

test_developer_delete(_Config) ->
    meck:new(database),
    meck:expect(database, delete_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = developer:delete([], <<"id">>),
    [{_, {database, delete_doc, [[], <<"id">>]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% AUTHORIZE
%% ###############################################################

test_developer_authorize_true(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    true = developer:authorize([], <<"id1">>, <<"baam">>),
    meck:validate(database),
    meck:unload(database).

test_developer_authorize_false(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {ok, ?DOC} end),
    false = developer:authorize([], <<"id1">>, <<"baaam">>),
    meck:validate(database),
    meck:unload(database).

test_developer_authorize_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_) -> {error, not_found} end),
    {error, not_found} = developer:authorize([], <<"id1">>, <<"baaam">>),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% LIST GAMES
%% ###############################################################

test_developer_list_games(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _, _) -> {ok, doc} end),
    {ok, doc} = developer:list_games([], <<"id">>),
    [{_, {database, read_doc, [[], {<<"games">>, <<"list">>},
        [{key, [<<"id">>]}], false]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% ###############################################################
%% ###############################################################
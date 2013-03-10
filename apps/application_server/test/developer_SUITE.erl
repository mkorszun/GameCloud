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
    test_developer_update_additional_elements,
    test_developer_update_missing_element,
    test_developer_update_empty_doc,
    test_developer_update_bad_format
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
    test_developer_list_games,
    test_developer_list_games_exclude
].

%% ###############################################################
%% TEST HELPERS
%% ###############################################################

-define(CREATE_DATA,
    {struct, [
        {<<"id">>, <<"id1">>},
        {<<"email">>, <<"a@b">>},
        {<<"password">>, <<"baam">>}
    ]}).

-define(DOC,
    {[
        {<<"_id">>, <<"id1">>},
        {<<"id">>, <<"id1">>},
        {<<"email">>, <<"a@b">>},
        {<<"type">>, <<"developer">>},
        {<<"password">>, <<"5886A61A13CB927813286ECDCEF509FD7A04535F">>}
    ]}).

%% ###############################################################
%% CREATE
%% ###############################################################

test_developer_create(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = developer:create([], ?CREATE_DATA),
    [{_,{database, save_doc, [[], Doc]}, {ok, doc}}] = meck:history(database),
    <<"developer">> = document:read(<<"type">>, Doc),
    <<"5886A61A13CB927813286ECDCEF509FD7A04535F">> = document:read(<<"password">>, Doc),
    <<"a@b">> = document:read(<<"email">>, Doc),
    <<"id1">> = document:read(<<"_id">>, Doc),
    <<"id1">> = document:read(<<"id">>, Doc),
    meck:validate(database),
    meck:unload(database).

test_developer_create_additional_elements(_Config) ->
    {error,{bad_data,no_extra_properties_allowed}} = developer:create([], struct:set_value(<<"a">>, <<"b">>, ?CREATE_DATA)).

test_developer_create_missing_element(_Config) ->
    {error,{bad_data,missing_required_property}} = developer:create([],{struct, []}),
    {error,{bad_data,missing_required_property}} = developer:create([],{struct, [{<<"email">>, <<"a@b">>}]}),
    {error,{bad_data,missing_required_property}} = developer:create([],{struct, [{<<"id">>, <<"id">>}]}),
    {error,{bad_data,missing_required_property}} = developer:create([],{struct, [{<<"id">>, <<"id">>}, {<<"email">>, <<"a@b">>}]}).

test_developer_create_empty_doc(_Config) ->
    {error,{bad_data,not_object}} = developer:create([], []),
    {error,{bad_data,not_object}} = developer:create([], {}),
    {error,{bad_data,not_object}} = developer:create([], a).

test_developer_create_bad_format(_Config) ->
    {error,{bad_data,not_string}} = developer:create([], {struct, [{<<"id">>, a}]}),
    {error,{bad_data,not_string}} = developer:create([], {struct, [{<<"id">>, <<"i">>}, {<<"email">>, b}]}),
    {error,{bad_data,not_string}} = developer:create([], {struct, [{<<"id">>, <<"i">>}, {<<"email">>, <<"e">>}, {<<"password">>, c}]}).

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
    {ok, doc} = developer:update([], <<"id">>, struct:set_value([{<<"password">>, <<"pass">>}, {<<"id">>, <<"666">>}], ?CREATE_DATA)),
    [{_, {database, read_doc, [[], <<"id">>]}, {ok, ?DOC}},
     {_, {database, save_doc, [[], Doc]}, {ok,doc}}] = meck:history(database),
    <<"developer">> = document:read(<<"type">>, Doc),
    <<"A340A2AC8CC8A7E2E7902F588FDF622575BB1937">> = document:read(<<"password">>, Doc),
    <<"a@b">> = document:read(<<"email">>, Doc),
    <<"id1">> = document:read(<<"_id">>, Doc),
    <<"id1">> = document:read(<<"id">>, Doc),
    meck:validate(database),
    meck:unload(database).

test_developer_update_additional_elements(_Config) ->
    {error,{bad_data,no_extra_properties_allowed}} = developer:update([], <<"id">>, struct:set_value(<<"a">>, <<"b">>, ?CREATE_DATA)).

test_developer_update_missing_element(_Config) ->
    {error,{bad_data,missing_required_property}} = developer:update([], <<"id">>, {struct, []}),
    {error,{bad_data,missing_required_property}} = developer:update([], <<"id">>, {struct, [{<<"email">>, <<"a@b">>}]}),
    {error,{bad_data,missing_required_property}} = developer:update([], <<"id">>, {struct, [{<<"id">>, <<"id">>}]}),
    {error,{bad_data,missing_required_property}} = developer:update([], <<"id">>, {struct, [{<<"id">>, <<"id">>}, {<<"email">>, <<"a@b">>}]}).

test_developer_update_empty_doc(_Config) ->
    {error,{bad_data,not_object}} = developer:update([], <<"id">>, []),
    {error,{bad_data,not_object}} = developer:update([], <<"id">>, {}),
    {error,{bad_data,not_object}} = developer:update([], <<"id">>, a).

test_developer_update_bad_format(_Config) ->
    {error,{bad_data,not_string}} = developer:update([], <<"id">>, {struct, [{<<"id">>, a}]}),
    {error,{bad_data,not_string}} = developer:update([], <<"id">>, {struct, [{<<"id">>, <<"i">>}, {<<"email">>, b}]}),
    {error,{bad_data,not_string}} = developer:update([], <<"id">>, {struct, [{<<"id">>, <<"i">>}, {<<"email">>, <<"e">>}, {<<"password">>, c}]}).

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
    meck:expect(database, read_doc, fun(_, _, _, _) -> {ok, [{[{<<"name">>, <<"a">>}]}]} end),
    {ok, [{[{<<"name">>, <<"a">>}]}]} = developer:list_games([], <<"id">>, []),
    [{_, {database, read_doc, [[], {<<"games">>, <<"list">>},
        [{key, [<<"id">>]}], false]}, {ok, [{[{<<"name">>, <<"a">>}]}]}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_developer_list_games_exclude(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _, _) -> {ok, [{[{<<"name">>, <<"a">>}]}, {[{<<"name">>, <<"b">>}]}]} end),
    {ok, [{[{<<"name">>, <<"a">>}]}]} = developer:list_games([], <<"id">>, [<<"b">>]),
    [{_, {database, read_doc, [[], {<<"games">>, <<"list">>},
        [{key, [<<"id">>]}], false]}, {ok, [{[{<<"name">>, <<"a">>}]}, {[{<<"name">>, <<"b">>}]}]}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% ###############################################################
%% ###############################################################
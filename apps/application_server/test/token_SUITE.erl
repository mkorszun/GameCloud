%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Token tests suite
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(token_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% ###############################################################
%% CT API
%% ###############################################################

all() -> [
    {group, create},
    {group, read},
    {group, validate}
].

groups() -> [
    {create, [], create()},
    {read, [], read()},
    {validate, [], validate()}
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
    test_token_create
].

read() -> [
    test_token_read
].

validate() -> [
    test_token_validate,
    test_token_validate_expired,
    test_token_validate_missing
].

%% ###############################################################
%% CREATE
%% ###############################################################

test_token_create(_Config) ->
    meck:new(database),
    meck:new(dateutils, [passthrough]),
    meck:new(authorization, [passthrough]),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    meck:expect(dateutils, timestamp_str, fun(_) -> <<"12345">> end),
    meck:expect(authorization, token, fun(_) -> <<"ABCDEFG">> end),
    {ok, doc} = token:create([], <<"developer_id">>),
    [{_,{database, save_doc, [[], Doc]}, {ok, doc}}] = meck:history(database),
    <<"12345">> = document:read(<<"timestamp">>, Doc),
    <<"ABCDEFG">> = document:read(<<"token">>, Doc),
    <<"token">> = document:read(<<"type">>, Doc),
    <<"developer_id">> = document:read(<<"developer_id">>, Doc),
    meck:validate(database),
    meck:validate(dateutils),
    meck:validate(authorization),
    meck:unload(database),
    meck:unload(dateutils),
    meck:unload(authorization).

%% ###############################################################
%% READ
%% ###############################################################

test_token_read(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, doc} end),
    {ok, doc} = token:read([], <<"developer_id">>, <<"token">>),
    [{_, {database, read_doc, [[], {<<"tokens">>, <<"read">>},
        [{key, [<<"developer_id">>, <<"token">>]}]]}, {ok, doc}}]
        = meck:history(database),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% VALIDATE
%% ###############################################################

test_token_validate(_Config) ->

    Doc = {[{<<"developer_id">>,<<"id">>},{<<"token">>,<<"t">>},
            {<<"timestamp">>, dateutils:timestamp_str(5)}]},

    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, Doc} end),
    meck:expect(database, save_doc, fun(_, _) -> {ok, doc} end),
    meck:new(dateutils, [passthrough]),
    meck:expect(dateutils, timestamp_str, fun(_) -> <<"12345">> end),

    {ok, doc} = token:validate([], <<"id">>, <<"t">>),

    [{_, {database, read_doc, [[], {<<"tokens">>, <<"read">>},
        [{key, [<<"id">>, <<"t">>]}]]}, {ok, Doc}},
     {_,{database, save_doc, [[], UpdatedDoc]}, {ok, doc}}
    ] = meck:history(database),

    <<"id">> = document:read(<<"developer_id">>, UpdatedDoc),
    <<"t">> = document:read(<<"token">>, UpdatedDoc),
    <<"12345">> = document:read(<<"timestamp">>, UpdatedDoc),

    meck:validate(database),
    meck:validate(dateutils),
    meck:unload(database),
    meck:unload(dateutils).


test_token_validate_expired(_Config) ->

    Doc = {[{<<"developer_id">>,<<"id">>},{<<"token">>,<<"t">>},
            {<<"timestamp">>, dateutils:timestamp_str(-5)}]},

    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {ok, Doc} end),
    {error, token_expired} = token:validate([], <<"id">>, <<"token">>),
    meck:validate(database),
    meck:unload(database).

test_token_validate_missing(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_, _, _) -> {error, not_found} end),
    {error, not_found} = token:validate([], <<"id">>, <<"token">>),
    meck:validate(database),
    meck:unload(database).

%% ###############################################################
%% ###############################################################
%% ###############################################################
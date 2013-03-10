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
    test_game_create_empty_doc,
    test_game_create_additional_elements,
    test_game_create_missing_element,
    test_game_create_wrong_format,
    test_game_create_wrong_enums
].

read() -> [
    test_game_read
].

read_screen() -> [
    test_game_read_screen
].

update() -> [
    test_game_update,
    test_game_update_empty_doc,
    test_game_update_additional_elements,
    test_game_update_missing_element,
    test_game_update_wrong_format,
    test_game_update_doc_not_found
].

delete() -> [
    test_game_delete,
    test_game_delete_not_found
].

path() -> [
    test_path_game,
    test_path_screen
].

%% ###############################################################
%% TEST HELPERS
%% ###############################################################

-define(CREATE_DATA,
    {struct, [
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
    ]}).

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

id(ok) -> {<<"developer_id">>, <<"a">>}; id(error) -> {<<"developer_id">>, a}.
name(ok) -> {<<"name">>, <<"b">>}; name(error) -> {<<"name">>, b}.
description(ok) -> {<<"description">>, <<"c">>}; description(error) -> {<<"description">>, c}.
platform(ok) -> {<<"platform">>, <<"ios">>}; platform(error) -> {<<"platform">>, ios}; platform(wrong) -> {<<"platform">>, <<"linux">>}.
game_link(ok) -> {<<"game_link">>, <<"d">>}; game_link(error) -> {<<"game_link">>, d}.
market_link(ok) -> {<<"market_link">>, <<"e">>}; market_link(error) -> {<<"market_link">>, e}.
tags(ok) -> {<<"tags">>, [<<"f">>]}; tags(error) -> {<<"tags">>, [f]}.
status(ok) -> {<<"status">>, <<"new">>}; status(error) -> {<<"status">>, new}; status(wrong) -> {<<"status">>, <<"baam">>}.
screen_name(ok) -> {<<"name">>, <<"g">>}; screen_name(error) -> {<<"name">>, g}.
screen_content_type(ok) -> {<<"content_type">>, <<"h">>}; screen_content_type(error) -> {<<"content_type">>, h}.
screen_content(ok) -> {<<"content">>, <<"i">>}; screen_content(error) -> {<<"content">>, i}.

%% ###############################################################
%% CREATE
%% ###############################################################

test_game_create(_Config) ->
    meck:new(database),
    meck:expect(database, save_doc, fun(_,_) -> {ok, doc} end),
    {ok, doc} = game:create([], ?CREATE_DATA),
    [{_,{database, save_doc, [[], Doc]}, {ok, doc}}] = meck:history(database),
    <<"game">> = document:read(<<"type">>, Doc),
    <<"id1">> = document:read(<<"developer_id">>, Doc),
    <<"wc3">> = document:read(<<"name">>, Doc),
    <<"baam">> = document:read(<<"description">>, Doc),
    <<"ios">> = document:read(<<"platform">>, Doc),
    <<"g_link">> = document:read(<<"game_link">>, Doc),
    <<"m_link">> = document:read(<<"market_link">>, Doc),
    [<<"one">>, <<"two">>] = document:read(<<"tags">>, Doc),
    <<"new">> = document:read(<<"status">>, Doc),
    Screen = document:read(<<"screen">>, Doc),
    <<"screen.jpg">> = document:read(<<"name">>, Screen),
    <<"image/jpg">> = document:read(<<"content_type">>, Screen),
    <<"kjkasdjksadjsa">> = document:read(<<"content">>, Screen),
    meck:validate(database),
    meck:unload(database).

test_game_create_empty_doc(_Config) ->
    {error,{bad_data,not_object}} = game:create([], []),
    {error,{bad_data,not_object}} = game:create([], {}),
    {error,{bad_data,not_object}} = game:create([], a).

test_game_create_additional_elements(_Config) ->
    {error,{bad_data,no_extra_properties_allowed}} = game:create([], struct:set_value(<<"a">>, <<"b">>, ?CREATE_DATA)).

test_game_create_missing_element(_Config) ->
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [name(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok)]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, []}}]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok)]}}]}),
    {error,{bad_data,missing_required_property}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(ok)]}}]}).

test_game_create_wrong_format(_Config) ->
    {error,{bad_data,not_string}} = game:create([],{struct, [id(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(error)]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(error)]}),
    {error,{bad_data,not_object}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {}}]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(error)]}}]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(error)]}}]}),
    {error,{bad_data,not_string}} = game:create([],{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(ok), screen_content(error)]}}]}).

test_game_create_wrong_enums(_Config) ->
    {error,{bad_data,not_in_enum}} = game:create([],[id(ok), name(ok), description(ok), platform(wrong)]),
    {error,{bad_data,not_in_enum}} = game:create([],[id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(wrong)]).

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
    {ok, doc} = game:update([], <<"id">>, <<"key">>, struct:set_value(<<"name">>, <<"baam">>, ?CREATE_DATA)),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {ok, ?DOC}},
     {_, {database, save_doc, [[], _]}, {ok, doc}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

test_game_update_empty_doc(_Config) ->
    {error, {bad_data, not_object}} = game:update([], <<"id">>, <<"key">>, []),
    {error, {bad_data, not_object}} = game:update([], <<"id">>, <<"key">>, {}),
    {error, {bad_data, not_object}} = game:update([], <<"id">>, <<"key">>, a).

test_game_update_additional_elements(_Config) ->
    {error,{bad_data,no_extra_properties_allowed}} = game:update([], <<"id">>, <<"key">>, struct:set_value(<<"a">>, <<"b">>, ?CREATE_DATA)).

test_game_update_missing_element(_Config) ->
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [name(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok)]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, []}}]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok)]}}]}),
    {error,{bad_data,missing_required_property}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(ok)]}}]}).

test_game_update_wrong_format(_Config) ->
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(error)]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(error)]}),
    {error,{bad_data,not_object}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {}}]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(error)]}}]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(error)]}}]}),
    {error,{bad_data,not_string}} = game:update([],<<"id">>,<<"key">>,{struct, [id(ok), name(ok), description(ok), platform(ok), game_link(ok), market_link(ok), tags(ok), status(ok), {<<"screen">>, {struct, [screen_name(ok), screen_content_type(ok), screen_content(error)]}}]}).

test_game_update_doc_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, read_doc, fun(_,_,_) -> {error, not_found} end),
    {error, not_found} = game:update([], <<"id">>, <<"key">>, ?CREATE_DATA),
    [{_, {database, read_doc, [[], {<<"games">>, <<"update">>}, [{key, [<<"id">>, <<"key">>]}]]}, {error, not_found}}] = meck:history(database),
    meck:validate(database),
    meck:unload(database).

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

test_game_delete_not_found(_Config) ->
    meck:new(database),
    meck:expect(database, delete_doc, fun(_,_,_) -> {error, not_found} end),
    {error, not_found} = game:delete([], <<"id">>, <<"key">>),
    [{_, {database, delete_doc, [[], {<<"games">>, <<"delete">>}, [{key, [<<"id">>, <<"key">>]}]]}, {error, not_found}}] = meck:history(database),
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
%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).
-export([register/2, exists/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "game"}).

%% ###############################################################
%% API
%% ###############################################################

register(DB, Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    Password = proplists:get_value("password", Args),
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            database:save_doc(DB, build_doc(Args));
        {error, Error} ->
            {error, Error}
    end.

exists(DB, DeveloperId, Password, GameUUID) ->
    case developer:authorize(DB, DeveloperId, Password) of
        {ok, true} ->
            do_exists(DB, DeveloperId, GameUUID);
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_exists(DB, DeveloperId, GameUUID) ->
    View = {<<"games">>, <<"by_developer">>},
    Keys = {key, views:keys([DeveloperId, GameUUID])},
    case database:exists(DB, View, [Keys]) of
        {ok, true} ->
            {ok, true};
        {error, not_found} ->
            {error, game_not_found};
        {error, Error} ->
            {error, Error}
    end.

build_doc(Args) ->
    DeveloperId = proplists:lookup("developer_id", Args),
    GameId = proplists:lookup("game_id", Args),
    Description = proplists:lookup("description", Args),
    document:create([DeveloperId, GameId, Description, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
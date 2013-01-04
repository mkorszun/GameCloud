%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game save business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(save).

-compile([{parse_transform, lager_transform}]).

-export([create/2, create/3]).
-export([read1/1, read1/2, read2/3, read2/4]).
-export([list1/1, list1/2, list2/1, list2/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% MACROS
%% ############################################################### 

-define(TYPE, {"type", "save"}).
-define(MAP, [{<<"_id">>, <<"save_key">>}, {<<"_attachments">>, <<"attachments">>}]).

%% ###############################################################
%% API
%% ############################################################### 

%% ###############################################################
%% CREATE
%% ############################################################### 

create(Args, Files) ->
    create(application_server_db:connection(), Args, Files).

create(DB, Args, Files) ->
    GameKey = proplists:get_value("game_key", Args),
    PlayerId = proplists:get_value("player_id", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize(DB, GameKey, PlayerId, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, creating save for game=~s",
                [PlayerId, GameKey]),
            database:save_doc(DB, build_doc(Args), Files);
        {error, Error} ->
            ?ERR("Failed to create save for player=~s and game=~s: ~p",
                [PlayerId, GameKey, Error]),
            {error, Error}
    end.

%% ###############################################################
%% READ
%% ############################################################### 

read1(Args) ->
    read1(application_server_db:connection(), Args).

read1(DB, Args) ->
    GameKey = proplists:get_value("game_key", Args),
    PlayerId = proplists:get_value("player_id", Args),
    Password = proplists:get_value("password", Args),
    SaveKey = proplists:get_value("save_key", Args),
    case player:authorize(DB, GameKey, PlayerId, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, reading save=~s",
                [PlayerId, SaveKey]),
            read2(DB, GameKey, PlayerId, SaveKey);
        {error, Error} ->
            ?ERR("Failed to read save=~s for player=~s: ~p",
                [SaveKey, PlayerId, Error]),
            {error, Error}
    end.

read2(GameKey, PlayerId, SaveKey) ->
    read2(application_server_db:connection(), GameKey, SaveKey).

read2(DB, GameKey, PlayerId, SaveKey) ->
    View = {<<"game_saves">>, <<"saves">>},
    Keys = {key, views:keys([GameKey, PlayerId, SaveKey])},
    case database:read_doc(DB, View, [Keys]) of
        {ok, [Doc]} ->
            ?DBG("Save=~s found, getting attachments",
                [SaveKey]),
            Doc1 = attachments:get(DB, Doc, true),
            Doc2 = document:delete(Doc1, [<<"_rev">>]),
            {ok, document:rename(Doc2, ?MAP)};
        {error, not_found} ->
            ?DBG("Failed to find save=~s",
                [SaveKey]),
            {error, save_not_found};
        {error, Error} ->
            ?DBG("Failed to find save=~s: ~p",
                [SaveKey, Error]),
            {error, Error}
    end.

%% ###############################################################
%% LIST
%% ############################################################### 

list1(Args) ->
    list1(application_server_db:connection(), Args).

list1(DB, Args) ->
    GameKey = proplists:get_value("game_key", Args),
    PlayerId = proplists:get_value("player_id", Args),
    Password = proplists:get_value("password", Args),
    case player:authorize(DB, GameKey, PlayerId, Password) of
        {ok, true} ->
            ?DBG("Player=~s authorized, listing saves", 
                [PlayerId]),
            list2(DB, PlayerId);
        {error, Error} ->
            ?ERR("Failed to list saves for player=~s: ~p", 
                [PlayerId, Error]),
            {error, Error}
    end.

list2(PlayerId) ->
    list2(application_server_db:connection(), PlayerId).

list2(DB, PlayerId) ->
    View = {<<"game_saves">>, <<"player_saves">>},
    Keys = {key, views:keys([PlayerId])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 
 
build_doc(Args) ->
    GameKey = proplists:lookup("game_key", Args),
    PlayerId = proplists:lookup("player_id", Args),
    SaveName = proplists:lookup("save_name", Args),
    Date = proplists:lookup("date", Args),
    document:create([GameKey, PlayerId, SaveName, Date, ?TYPE]).

%% ###############################################################
%% 
%% ###############################################################
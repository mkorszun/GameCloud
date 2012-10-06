%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Player business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(player).
-export([register/2, delete/2, authorize/2]).

%% ###############################################################
%% MACORS
%% ############################################################### 

-define(TYPE, {"type", "player"}).

%% ###############################################################
%% API
%% ############################################################### 

register(DB, Args) ->
    case game:exists(DB, proplists:get_value("game_uuid", Args)) of
        {ok, true} ->
            database:save_doc(DB, build_doc(Args));
        {error, Error} ->
            {error, Error}
    end.

delete(DB, Args) ->
    case authorize(DB, Args) of
        {ok, true} ->
            do_delete(DB, Args);
        {error, Error} ->
            {error, Error}
    end.

authorize(DB, Args) ->
    case database:read_doc(DB, proplists:get_value("player_uuid", Args)) of
        {ok, Doc} ->
            authorization:authorize(player, Doc, Args);
        {error, not_found} ->
            {error, player_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_delete(DB, Args) ->
    case database:delete_doc(DB, proplists:get_value("player_uuid", Args)) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            {error, player_not_found};
        {error, Error} ->
            {error, Error}
    end.

build_doc(Args) ->
    document:create([?TYPE | Args]).

%% ###############################################################
%% 
%% ############################################################### 
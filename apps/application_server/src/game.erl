%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game).
-export([register/2, exists/2]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "game"}).

%% ###############################################################
%% API
%% ###############################################################

register(DB, Args) ->
    case developer:authorize(DB, Args) of
        {ok, true} ->
            database:save_doc(DB, build_doc(Args));
        {error, Error} ->
            {error, Error}
    end.

exists(DB, GameUUID) ->
    case database:exists(DB, GameUUID) of
        {ok, true} ->
            {ok, true};
        {error, not_found} ->
            {error, game_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Args) ->
    Params = parameter:delete(["password"], Args),
    document:create([?TYPE | Params]).

%% ###############################################################
%% 
%% ###############################################################
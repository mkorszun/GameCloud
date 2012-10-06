%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game save business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(save).
-export([register/3, read/2, delete/2]).

%% ###############################################################
%% MACROS
%% ############################################################### 

-define(TYPE, {"type", "save"}).

%% ###############################################################
%% API
%% ############################################################### 

register(DB, Params, Files) ->
    case player:authorize(DB, Params) of
        {ok, true} ->
            database:save_doc(DB, build_doc(Params), Files);
        {error, Error} ->
            {error, Error}
    end.

read(DB, Args) ->
    case player:authorize(DB, Args) of
        {ok, true} ->
            do_read(DB, Args);
        {error, Error} ->
            {error, Error}
    end.

delete(DB, Args) ->
    case player:authorize(DB, Args) of
        {ok, true} ->
            do_delete(DB, Args);
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

do_read(DB, Args) ->
    case database:read_doc(DB, proplists:get_value("save_uuid", Args)) of
        {ok, Doc} ->
            {ok, attachments:get(DB, Doc, true)};
        {error, not_found} ->
            {error, save_not_found};
        {error, Error} ->
            {error, Error}
    end.

do_delete(DB,Args) ->
    case database:delete_doc(DB, proplists:get_value("save_uuid", Args)) of
        {ok, Doc} ->
            {ok, Doc};
        {error, not_found} ->
            {error, save_not_found};
        {error, Error} ->
            {error, Error}
    end.

build_doc(Args) ->
    Doc = parameter:delete(["password"], Args),
    document:create([?TYPE | Doc]).

%% ###############################################################
%% 
%% ###############################################################
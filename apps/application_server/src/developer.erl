%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer).
-export([register/2, authorize/2]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "developer"}).

%% ###############################################################
%% API
%% ###############################################################

register(DB, Args) ->
    case database:save_doc(DB, build_doc(Args)) of
        {ok, CreatedDoc} ->
            {ok, CreatedDoc};
        {error, conflict} ->
            {error, developer_already_exists};
        {error, Error} ->
            {error, Error}
    end.

authorize(DB, Args) ->
    case database:read_doc(DB, proplists:get_value("developer_id", Args)) of
        {ok, Doc} ->
            authorization:authorize(developer, Doc, Args);
        {error, not_found} ->
            {error, developer_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Args) ->
    Id = proplists:get_value("developer_id", Args),
    Params = proplists:delete("developer_id", Args),
    document:create([{"_id", Id}, ?TYPE | Params]).

%% ###############################################################
%%
%% ###############################################################
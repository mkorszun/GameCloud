%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer).
-export([register/2, authorize/3]).

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

authorize(DB, DeveloperId, Password) ->
    case database:read_doc(DB, DeveloperId) of
        {ok, Doc} ->
            authorization:authorize(developer, Doc, DeveloperId, Password);
        {error, not_found} ->
            {error, developer_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_doc(Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    Password = proplists:get_value("password", Args),
    Email = proplists:get_value("email", Args),
    PasswordHash = cryptography:sha(Password, DeveloperId),
    document:create([{"_id", DeveloperId}, {"password", PasswordHash}, {"email", Email}, ?TYPE]).

%% ###############################################################
%%
%% ###############################################################
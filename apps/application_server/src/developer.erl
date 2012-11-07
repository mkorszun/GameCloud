%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Developer business logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(developer).

-compile([{parse_transform, lager_transform}]).

-export([create/1, authorize/2]).
-export([create/2, authorize/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "developer"}).

%% ###############################################################
%% API
%% ###############################################################

create(Args) ->
    create(application_server_db:connection(), Args).

create(DB, Args) ->
    case database:save_doc(DB, build_doc(Args)) of
        {ok, CreatedDoc} ->
            Id = proplists:get_value("developer_id", Args),
            ?INF("Developer=~s created", [Id]),
            {ok, CreatedDoc};
        {error, conflict} ->
            Id = proplists:get_value("developer_id", Args),
            ?ERR("Developer=~s already exists", [Id]),
            {error, developer_already_exists};
        {error, Error} ->
            ?ERR("Failed to create developer=~s: ~p", [Error]),
            {error, Error}
    end.

authorize(DeveloperId, Password) ->
    authorize(application_server_db:connection(), DeveloperId, Password).

authorize(DB, DeveloperId, Password) ->
    case database:read_doc(DB, DeveloperId) of
        {ok, Doc} ->
            ?DBG("Developer=~s found, authorizing", [DeveloperId]),
            authorization:authorize(developer, Doc, DeveloperId, Password);
        {error, not_found} ->
            ?ERR("Developer=~s not found", [DeveloperId]),
            {error, developer_not_found};
        {error, Error} ->
            ?ERR("Failed to authorize developer=~s: ~p", [DeveloperId, Error]),
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
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
            lager:info("Developer ~p created", [Id]),
            {ok, CreatedDoc};
        {error, conflict} ->
            Id = proplists:get_value("developer_id", Args),
            lager:error("Developer ~p already exists", [Id]),
            {error, developer_already_exists};
        {error, Error} ->
            lager:error("Failed to create developer: ~s", [Error]),
            {error, Error}
    end.

authorize(DeveloperId, Password) ->
    authorize(application_server_db:connection(), DeveloperId, Password).

authorize(DB, DeveloperId, Password) ->
    case database:read_doc(DB, DeveloperId) of
        {ok, Doc} ->
            do_authorize(Doc, DeveloperId, Password);
        {error, not_found} ->
            lager:error("Developer ~p not found", [DeveloperId]),
            {error, developer_not_found};
        {error, Error} ->
            lager:error("Failed to authorize developer: ~s", [Error]),
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_authorize(Doc, DeveloperId, Password) ->
    case authorization:authorize(developer, Doc, DeveloperId, Password) of
        {ok, true} ->
            lager:info("Developer ~p authorized", [DeveloperId]),
            {ok, true};
        {error, unauthorized} ->
            lager:error("Developer ~p unauthorized", [DeveloperId]),
            {error, unauthorized};
        {error, Error} ->
            lager:error("Failed to authorize developer: ~s", [DeveloperId]),
            {error, Error}
    end.

build_doc(Args) ->
    DeveloperId = proplists:get_value("developer_id", Args),
    Password = proplists:get_value("password", Args),
    Email = proplists:get_value("email", Args),
    PasswordHash = cryptography:sha(Password, DeveloperId),
    document:create([{"_id", DeveloperId}, {"password", PasswordHash}, {"email", Email}, ?TYPE]).

%% ###############################################################
%%
%% ###############################################################
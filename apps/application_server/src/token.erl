%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Token logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(token).

-export([create/1, create/2]).
-export([read/2, read/3]).
-export([validate/2, validate/3]).

-define(VALIDITY, 15).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(DeveloperId) ->
    create(application_server_db:connection(), DeveloperId).

create(DB, DeveloperId) ->
    database:save_doc(DB, build_token(DeveloperId)).

%% ###############################################################
%% READ
%% ###############################################################

read(DeveloperId, Token) ->
    read(application_server_db:connection(), DeveloperId, Token).

read(DB, DeveloperId, Token) ->
    View = {<<"tokens">>, <<"read">>},
    Keys = {key, views:keys([DeveloperId, Token])},
    database:read_doc(DB, View, [Keys]).

%% ###############################################################
%% VALIDATE TOKEN
%% ###############################################################

validate(DeveloperId, Token) ->
    validate(application_server_db:connection(), DeveloperId, Token).
validate(DB, DeveloperId, Token) ->
    case read(DB, DeveloperId, Token) of
        {ok, Doc} ->
            case dateutils:valid(document:read(<<"timestamp">>, Doc)) of
                true ->
                    reset_token(DB, Doc);
                false ->
                    {error, token_expired}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

reset_token(DB, Doc) ->
    Timestamp = dateutils:timestamp_str(?VALIDITY),
    Updated = document:set_value(<<"timestamp">>, Timestamp, Doc),
    database:save_doc(DB, Updated).

build_token(DeveloperId) ->
    Token = authorization:token(DeveloperId),
    Timestamp = dateutils:timestamp_str(?VALIDITY),
    {[{<<"developer_id">>, DeveloperId}, {<<"token">>, Token},
      {<<"timestamp">>, Timestamp}, {<<"type">>, <<"token">>}]}.

%% ###############################################################
%% ###############################################################
%% ###############################################################
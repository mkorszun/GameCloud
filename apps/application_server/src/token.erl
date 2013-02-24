%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Token logic
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(token).

-export([create/1, create/2, read/3, check_token/2, check_token/3]).

%% ###############################################################
%% API
%% ###############################################################

%% ###############################################################
%% CREATE
%% ###############################################################

create(DeveloperId) ->
    create(application_server_db:connection(), DeveloperId).

create(DB, DeveloperId) ->
    Timestamp = int_to_bin(dateutils:timestamp(5)),
    Doc = {[{<<"developer_id">>, DeveloperId}, {<<"token">>, token(DeveloperId)},
        {<<"timestamp">>, Timestamp}, {<<"type">>, <<"token">>}]},
    database:save_doc(DB, Doc).

read(DB, DeveloperId, Token) ->
    View = {<<"tokens">>, <<"read">>},
    Keys = {key, views:keys([DeveloperId, Token])},
    database:read_doc(DB, View, [Keys]).

check_token(DeveloperId, Token) ->
  check_token(application_server_db:connection(), DeveloperId, Token).
check_token(DB, DeveloperId, Token) ->
    case read(DB, DeveloperId, Token) of
      {ok, Doc} ->
          case expired(document:read(<<"timestamp">>, Doc)) of
              true ->
                  reset_token(DB, Doc);
              false ->
                  {error, token_expired}
          end;
      {error, Error} ->
          {error, Error}
    end.

reset_token(DB, Doc) ->
    Timestamp = int_to_bin(dateutils:timestamp(5)),
    Updated = document:set_value(<<"timestamp">>, Timestamp, Doc),
    database:save_doc(DB, Updated).

token(DeveloperId) ->
    Key = list_to_binary(integer_to_list(
        crypto:rand_uniform(170141183460469231731687303715884105729,
                            340282366920938463463374607431768211455))),
    authorization:sha(DeveloperId, Key).

int_to_bin(I) ->
    list_to_binary(integer_to_list(I)).

bin_to_int(B) ->
    list_to_integer(binary_to_list(B)).

expired(Validity) when is_binary(Validity) ->
    expired(bin_to_int(Validity));
expired(Validity) ->
    Validity > dateutils:timestamp().

%% ###############################################################
%% ###############################################################
%% ###############################################################
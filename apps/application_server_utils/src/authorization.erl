%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([authorize/3, sha/2, token/1]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(R1, 170141183460469231731687303715884105729).
-define(R2, 340282366920938463463374607431768211455).

%% ###############################################################
%%
%% ###############################################################

authorize(Doc, Id, Password) ->
    Password1 = document:read(<<"password">>, Doc),
    auth(Password1, sha(Password, Id)).

sha(Data, Salt) ->
    C1 = crypto:sha_init(),
    C2 = crypto:sha_update(C1, Data),
    C3 = crypto:sha_update(C2, Salt),
    bin_to_hexstr(crypto:sha_final(C3)).

token(Data) ->
    sha(Data, integer_to_list(crypto:rand_uniform(?R1, ?R2))).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> true;
auth(_, _) -> false.

bin_to_hexstr(Bin) ->
    list_to_binary(lists:flatten(
        [io_lib:format("~2.16.0B", [X]) ||
            X <- binary_to_list(Bin)])).

%% ###############################################################
%% ###############################################################
%% ###############################################################
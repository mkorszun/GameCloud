%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([authorize/3]).

%% ###############################################################
%%
%% ###############################################################

authorize(Doc, Id, Password) ->
    Password1 = document:read(<<"password">>, Doc),
    auth(Password1, cryptography:sha(Password, Id)).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> true;
auth(_, _) -> false.

%% ###############################################################
%% ###############################################################
%% ###############################################################    

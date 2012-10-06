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

authorize(player, Doc, Args) ->
    Password1 = document:read(<<"password">>, Doc),
    Password2 = proplists:get_value("password", Args),
    auth(Password1, Password2);

authorize(developer, Doc, Args) ->
    Password1 = document:read(<<"password">>, Doc),
    Password2 = proplists:get_value("password", Args),
    auth(Password1, Password2).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> {ok, true};
auth(_, _) -> {error, unauthorized}.

%% ###############################################################
%% ###############################################################
%% ###############################################################    

%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([authorize/3, authorize/4]).

%% ###############################################################
%%
%% ###############################################################

authorize(player, Doc, Args) ->
    Password1 = document:read(<<"password">>, Doc),
    Password2 = proplists:get_value("password", Args),
    PlayerId = proplists:get_value("player_uuid", Args),
    auth(Password1, cryptography:sha(Password2, PlayerId));

authorize(developer, Doc, Args) ->
    Password1 = document:read(<<"password">>, Doc),
    Password2 = proplists:get_value("password", Args),
    DeveloperId = proplists:get_value("developer_id", Args),
    auth(Password1, cryptography:sha(Password2, DeveloperId)).

authorize(player, Doc, PlayerUUID, Password) ->
    Password1 = document:read(<<"password">>, Doc),
    auth(Password1, cryptography:sha(Password, PlayerUUID));

authorize(developer, Doc, DevId, DevPass) ->
    Password1 = document:read(<<"password">>, Doc),
    auth(Password1, cryptography:sha(DevPass, DevId)).

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

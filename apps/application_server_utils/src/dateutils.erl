%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Timestamp utils
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(dateutils).

-export([timestamp/0, timestamp/1]).
-export([timestamp_str/0, timestamp_str/1]).
-export([valid/1]).

%% ###############################################################
%% UTILS
%% ###############################################################

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega * 1000000 + Secs.

timestamp(Shift) ->
    timestamp() + 60 * Shift.

timestamp_str() ->
    int_to_bin(timestamp()).

timestamp_str(Shift) ->
    int_to_bin(timestamp(Shift)).

valid(Validity) when is_binary(Validity) ->
    valid(bin_to_int(Validity));
valid(Validity) ->
    Validity > timestamp().

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

int_to_bin(I) ->
    list_to_binary(integer_to_list(I)).

bin_to_int(B) ->
    list_to_integer(binary_to_list(B)).

%% ###############################################################
%% ###############################################################
%% ###############################################################
%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session server API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_server).

-export([start_session/2, stop_session/2, ping/2]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include("game_session_server.hrl").

%% ###############################################################
%% API
%% ###############################################################

start_session(GameKey, Token) ->
    {ok, Pid} = supervisor:start_child(game_session_server_sup, [GameKey, Token]),
    game_session_counter:add(?B2A(GameKey), Pid).

stop_session(_GameKey, Token) ->
    gen_fsm:send_event(?B2A(Token), stop).

ping(_GameKey, Token) ->
    gen_fsm:send_event(?B2A(Token), ping).

%% ###############################################################
%% ###############################################################
%% ###############################################################
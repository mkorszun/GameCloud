%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session server API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_server).

-export([start_session/2, stop_session/2, ping/2]).

%% ###############################################################
%% API
%% ###############################################################

start_session(GameKey, Token) ->
    Name = list_to_atom(GameKey ++ Token),
    {ok, Pid} = supervisor:start_child(game_session_server_sup, [Name]),
    game_session_counter:add(list_to_atom(GameKey), Pid).


stop_session(GameKey, Token) ->
    Name = list_to_atom(GameKey ++ Token),
    gen_fsm:send_event(Name, stop).

ping(GameKey, Token) ->
    Name = list_to_atom(GameKey ++ Token),
    gen_fsm:send_event(Name, ping).

%% ###############################################################
%% ###############################################################
%% ###############################################################
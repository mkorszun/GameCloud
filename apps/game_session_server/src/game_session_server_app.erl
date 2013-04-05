%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session server application
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_server_app).

-behaviour(application).
-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_StartType, _StartArgs) ->
    game_session_server_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################
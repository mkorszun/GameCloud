%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% API application
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_api_app).
-behaviour(application).

-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_Type, _StartArgs) ->
    game_cloud_api_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%%
%% ###############################################################
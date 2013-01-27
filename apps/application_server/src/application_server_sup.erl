%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Supervisor
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(application_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ###############################################################
%% API FUNCTIONS
%% ###############################################################

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ###############################################################
%% SUPERVISOR CALLBACKS
%% ###############################################################

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% ###############################################################
%% ###############################################################
%% ###############################################################
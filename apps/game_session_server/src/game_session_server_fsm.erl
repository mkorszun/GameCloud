%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session fsm
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_server_fsm).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, 'STARTED'/2, 'ACTIVE'/2, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% API
%% ###############################################################

start_link(Name) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [], []).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init(_) ->
    {ok, 'STARTED', [{timeout, 50000}], 50000}.

'STARTED'(ping, [{timeout, Timeout} | _] = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'STARTED'(timeout, State) ->
    {stop, normal, State};
'STARTED'(stop, State) ->
    {stop, normal, State}.

'ACTIVE'(ping, [{timeout, Timeout} | _] = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'ACTIVE'(timeout, State) ->
    {stop, normal, State};
'ACTIVE'(stop, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################
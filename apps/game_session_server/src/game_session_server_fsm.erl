%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session fsm
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_server_fsm).
-behaviour(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-export([start_link/2]).
-export([init/1, 'STARTED'/2, 'ACTIVE'/2, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include("game_session_server.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {id, key, timeout=5000, start_time}).

%% ###############################################################
%% API
%% ###############################################################

start_link(GameKey, SessionId) ->
    gen_fsm:start_link({local, ?B2A(SessionId)}, ?MODULE, [SessionId, GameKey], []).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init([SessionId, GameKey]) ->
    {ok, 'STARTED', #state{id = SessionId, key = GameKey, start_time = erlang:now()}, 5000}.

'STARTED'(ping, #state{timeout=Timeout} = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'STARTED'(timeout, State) ->
    {stop, normal, State};
'STARTED'(stop, State) ->
    {stop, normal, State}.

'ACTIVE'(ping, #state{timeout=Timeout} = State) ->
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

terminate(_Reason, _StateName, #state{id = Id, key = GameKey, start_time = Start}) ->
    SessionTime = timer:now_diff(erlang:now(), Start) div 1000000,
    case game_session_stats:save(GameKey, session_time, Id, ?I2B(SessionTime)) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERR("Failed to save session time - ~s ~s ~p: ~p",
                [GameKey, Id, SessionTime, Reason]),
            ok
    end.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################
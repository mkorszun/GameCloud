%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session counter
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_counter_gen).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ###############################################################
%% API
%% ###############################################################

start_link(GameKey) ->
    gen_server:start_link({local, GameKey}, ?MODULE, [], []).

%% ###############################################################
%% GEN_SERVER CALLBACKS
%% ###############################################################

init([]) ->
    {ok, [{counter, 0}]}.

handle_call(get_counter, _From, [{counter, Cnt}|_]=State) ->
    {reply, Cnt, State}.

handle_cast({add, SessionProcess}, [{counter, Cnt}|T]) ->
    erlang:monitor(process, SessionProcess),
    {noreply, [{counter, Cnt+1}|T]};
handle_cast(remove, [{counter, Cnt}|T]) ->
    {noreply, [{counter, Cnt-1}|T]}.

handle_info({'DOWN', _, _, _, _}, [{counter, 1}|_]=State) ->
    {stop, normal, State};
handle_info({'DOWN', _, _, _, _}, [{counter, Cnt}|T]) ->
    {noreply, [{counter, Cnt-1}|T]};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################
%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Game session counter API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_session_counter).

-export([add/2, count/1]).

%% ###############################################################
%% API
%% ###############################################################

add(GameKey, SessionProcess) ->
    supervisor:start_child(game_session_counter_sup, [GameKey]),
    gen_server:cast(GameKey, {add, SessionProcess}).

count(GameKey) ->
    try gen_server:call(GameKey, get_counter) of
        Cnt -> Cnt
    catch
        _:_ -> 0
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Parameters functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(parameter).
-export([delete/2]).

%% ###############################################################
%% 
%% ###############################################################

delete([], Proplist) -> Proplist;
delete([K | T], Proplist) ->
    delete(T, proplists:delete(K, Proplist)).

%% ###############################################################
%% ############################################################### 
%% ###############################################################



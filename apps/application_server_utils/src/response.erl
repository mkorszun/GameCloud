%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Response encoding functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(response).

-export([to_json/1]).

to_json(Res) when is_list(Res) ->
    to_json(list_to_binary(Res));
to_json(Res) ->
    couchbeam_ejson:encode([{res, Res}]).

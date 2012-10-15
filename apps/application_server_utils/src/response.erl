%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Response encoding functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(response).

-export([ok/1, error/1, to_json/2]).

ok(Res) ->
	to_json(ok, Res).

error(Res) ->
	to_json(error, Res).

to_json(Key, Res) when is_list(Res) ->
    to_json(Key, list_to_binary(Res));
to_json(Key, Res) ->
    couchbeam_ejson:encode({[{Key, Res}]}).

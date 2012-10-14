%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Views helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(views).

-export([keys/1]).

keys([]) -> [];
keys([H|T]) when is_binary(H) -> [H | keys(T)];
keys([H|T]) when is_list(H) -> [list_to_binary(H) | keys(T)];
keys(Element) when is_list(Element) -> list_to_binary(Element);
keys(Element) when is_binary(Element) -> Element.
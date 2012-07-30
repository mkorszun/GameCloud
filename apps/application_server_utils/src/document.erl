%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/1, read/2, get_id/1, view_keys/1, view_keys/2]).

create(Doc) -> 
    {doc(Doc)}.

read(Key, Doc) when is_list(Key) ->
    read(list_to_binary(Key), Doc);
read(Key, Doc) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc).

get_id(Doc) ->
    couchbeam_doc:get_id(Doc).    

view_keys(Keys) ->
    {key, keys(Keys)}.

view_keys(Names, L) when is_list(L) ->
    view_keys([proplists:get_value(N, L) || N <- Names]); 
view_keys(Names, Doc)  ->
    view_keys([read(K, Doc) || K <- Names]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

keys([]) -> [];
keys([H|T]) when is_binary(H) -> [H | keys(T)];
keys([H|T]) when is_list(H) -> [list_to_binary(H) | keys(T)].

doc([]) -> [];
doc([{K,V}|T]) -> [{bin(K), bin(V)} | doc(T)].

bin(Val) when is_binary(Val) -> Val;
bin(Val) when is_list(Val) -> list_to_binary(Val).

%% ###############################################################
%% ###############################################################
%% ############################################################### 

%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/1, read/2, read/3, get_id/1, delete/2, set_value/3, rename/2]).

%% ###############################################################
%%
%% ############################################################### 

create(Doc) when is_list(Doc) -> doc(Doc);
create({L} = Doc) when is_list(L) -> Doc.

read(Key, Doc) when is_list(Key) ->
    read(list_to_binary(Key), Doc);
read(Key, Doc) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc).

read(Key, Doc, Default) when is_list(Key) ->
    read(list_to_binary(Key), Doc, Default);
read(Key, Doc, Default) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc, Default).

get_id(Doc) ->
    couchbeam_doc:get_id(Doc).    

delete(Doc, []) ->
    Doc;
delete(Doc, [H|T]) ->
    delete(couchbeam_doc:delete_value(H, Doc), T).

set_value(Key, Value, Doc) when is_list(Value) ->
    set_value(Key, list_to_binary(Value), Doc);
set_value(Key, Value, Doc) ->
    couchbeam_doc:set_value(Key, Value, Doc).

rename(Doc, []) ->
    Doc;
rename(Doc, [{Old, New} | T]) ->
    Val = read(Old, Doc),
    Doc1 = delete(Doc, [Old]),
    rename(set_value(New, Val, Doc1), T).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

doc(Doc) -> doc(Doc, []).

doc([], Doc) -> {Doc};
doc([{K,V}|T], Doc) -> doc(T, [{key(K), val(V)} | Doc]).

key(K) when is_binary(K) -> K;
key(K) when is_list(K) -> list_to_binary(K).

val(V) when is_binary(V) -> V;
val([{_,_}|_] = Val) -> doc(Val);
val([H|_] = Val) when is_list(H) -> [val(X) || X <- Val];
val(Val) when is_list(Val) -> list_to_binary(Val).

%% ###############################################################
%% ###############################################################
%% ############################################################### 

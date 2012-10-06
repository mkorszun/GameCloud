%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/1, read/2, read/3, get_id/1, delete/2, set_value/3]).
-export([exists/2]).

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

set_value(Key, Value, Doc) ->
    couchbeam_doc:set_value(Key, Value, Doc).

exists(_, []) ->
    false;
exists(KV, [{_} = Doc | T]) ->
    case exists(KV, Doc) of
        true -> true;
        false -> exists(KV, T)
    end;
            
exists(KV, {Doc}) when is_list(Doc) ->
    exists(KV, Doc);
exists({Key, Value}, Doc) when is_list(Doc), is_list(Key), is_list(Value) ->
    exists({list_to_binary(Key), list_to_binary(Value)}, Doc);
exists({Key, Value}, Doc) when is_binary(Key), is_binary(Value) ->
    case proplists:get_value(Key, Doc) of
        Value -> true;
        _ -> false
    end.        

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

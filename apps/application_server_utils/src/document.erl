%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/1, read/2, get_id/1, delete/2, set_value/3]).

%% ###############################################################
%%
%% ############################################################### 

create(Doc) when is_list(Doc) -> {doc(Doc)};
create({L} = Doc) when is_list(L) -> Doc.

read(Key, Doc) when is_list(Key) ->
    read(list_to_binary(Key), Doc);
read(Key, Doc) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc).

get_id(Doc) ->
    couchbeam_doc:get_id(Doc).    

delete(Doc, []) ->
    Doc;
delete(Doc, [H|T]) ->
    delete(couchbeam_doc:delete_value(H, Doc), T).

set_value(Key, Value, Doc) ->
    couchbeam_doc:set_value(Key, Value, Doc).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

doc([]) -> [];
doc([{K,V}|T]) -> [{bin(K), bin(V)} | doc(T)].

bin(Val) when is_binary(Val) -> Val;
bin(Val) when is_list(Val) -> list_to_binary(Val).

%% ###############################################################
%% ###############################################################
%% ############################################################### 

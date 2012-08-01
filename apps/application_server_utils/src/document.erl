%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/1, read/2, get_id/1]).

create(Doc) -> 
    {doc(Doc)}.

read(Key, Doc) when is_list(Key) ->
    read(list_to_binary(Key), Doc);
read(Key, Doc) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc).

get_id(Doc) ->
    couchbeam_doc:get_id(Doc).    

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

%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([build_doc/3, read/2, read/3, get_id/1, delete/2, set_value/3, update_doc/3]).

%% ###############################################################
%%
%% ###############################################################

%% ###############################################################
%% BUILD DOCUMENT
%% ###############################################################

build_doc([], Doc, _) -> Doc;
build_doc([{K, V} |T], Doc, Mapping) ->
    {Key, Format} = proplists:get_value(K, Mapping),
    build_doc(T, [{Key, Format(V)} | Doc], Mapping).

%% ###############################################################
%% READ ELEMENT
%% ###############################################################

read(Key, Doc) when is_list(Key) ->
    read(list_to_binary(Key), Doc);
read(Key, Doc) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc).

read(Key, Doc, Default) when is_list(Key) ->
    read(list_to_binary(Key), Doc, Default);
read(Key, Doc, Default) when is_binary(Key) ->
    couchbeam_doc:get_value(Key, Doc, Default).

%% ###############################################################
%% GET DOCUMENT ID
%% ###############################################################

get_id(Doc) ->
    couchbeam_doc:get_id(Doc).

%% ###############################################################
%% DELETE KEYS FROM DOCUMENT
%% ###############################################################

delete(Doc, []) ->
    Doc;
delete(Doc, [H|T]) ->
    delete(couchbeam_doc:delete_value(H, Doc), T).

%% ###############################################################
%% SET VALUE
%% ###############################################################

set_value(Key, Value, Doc) when is_list(Value) ->
    set_value(Key, list_to_binary(Value), Doc);
set_value(Key, Value, Doc) ->
    couchbeam_doc:set_value(Key, Value, Doc).

%% ###############################################################
%% UPDATE DOCUMENT
%% ###############################################################

update_doc(OldDoc, NewDoc, Mapping) ->
    lists:foldl(
            fun({K,V}, D) ->
                case proplists:get_value(K, Mapping) of
                    {KK, F} ->
                        document:set_value(KK, F(V), D);
                    undefined ->
                        D
                end
            end,
            OldDoc,
            NewDoc
        ).

%% ###############################################################
%% ###############################################################
%% ###############################################################

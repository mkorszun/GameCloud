%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Document helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(document).
-export([create/3, update/3, read/2, read/3, get_id/1, delete/2, set_value/3, exclude/3]).
-export([build_doc/2, to_json/1]).
%% ###############################################################
%%
%% ###############################################################

-define(A(L), try list_to_existing_atom(L) of A -> A catch _:_ -> list_to_atom(L) end).

%% ###############################################################
%% CREATE DOCUMENT
%% ###############################################################

create(_, Doc, []) -> Doc;
create(Data, Doc, [{K1, {K2, Format}} | T]) ->
    case proplists:get_value(K1, Data) of
        undefined ->
            throw(?A(lists:flatten(io_lib:format("missing_~s", [K1]))));
        V ->
            create(Data, [{K2, Format(V)} | Doc], T)
    end.

%% ###############################################################
%% UPDATE DOCUMENT
%% ###############################################################

update(OldDoc, NewDoc, Mapping) ->
    lists:foldl(
            fun({K1, {K2, F}}, D) ->
                case proplists:get_value(K1, NewDoc) of
                    undefined ->
                        D;
                    V ->
                        document:set_value(K2, F(V), D)
                end
            end,
            OldDoc,
            Mapping
        ).

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
%% EXCLUDE DOCUMENTS BASED ON VALUE
%% ###############################################################

exclude(Docs, Exclude, Key) ->
    lists:filter(fun(Doc) ->
        N = document:read(Key, Doc),
        case lists:any(
            fun(M) when M =:= N ->
                    true;
               (_) ->
                    false
            end, Exclude) of
                false ->
                    true;
                true ->
                    false
        end
    end, Docs).

%% ###############################################################
%% JSON TO DOC
%% ###############################################################

build_doc(mochijson2 = M, Doc) when is_list(Doc) ->
    lists:map(fun(E) -> build_doc(M, E) end, Doc);
build_doc(mochijson2 = M, {array, Doc}) ->
    build_doc(M, Doc);
build_doc(mochijson2 = M, {struct, Doc}) ->
    {build_doc(M, Doc)};
build_doc(mochijson2 = M, {K,V}) ->
    {K, build_doc(M, V)};
build_doc(mochijson2, E) ->
    E.

to_json(Doc) when is_list(Doc) ->
    lists:map(fun(E) -> to_json(E) end, Doc);
to_json({Doc}) ->
    {struct, to_json(Doc)};
to_json({K, V}) ->
    {K, to_json(V)};
to_json(E) ->
    E.


%% ###############################################################
%% ###############################################################
%% ###############################################################

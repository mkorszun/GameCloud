%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Database helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(database).
-export([open/1, open/3, save_doc/2, save_doc/3, read_doc/2, read_doc/3,
         read_doc/4, delete_doc/2, delete_doc/3, exists/2, exists/3]).

%% ###############################################################
%%
%% ###############################################################

open(DBName) ->
    Conn = couchbeam:server_connection(),
    couchbeam:open_db(Conn, DBName).

open(DBName, Host, Port) ->
    Conn = couchbeam:server_connection(Host, Port),
    couchbeam:open_db(Conn, DBName).

save_doc(DB, Doc) ->
    couchbeam:save_doc(DB, Doc).

save_doc(DB, Doc, Attachments) ->
    Doc1 = set_attachments(Doc, Attachments),
    couchbeam:save_doc(DB, Doc1).

read_doc(DB, DocId) when is_binary(DocId) ->
    couchbeam:open_doc(DB, DocId);
read_doc(DB, DocId) when is_list(DocId) ->
    read_doc(DB, list_to_binary(DocId));
read_doc(_, _) -> {error, wrong_id}.

read_doc(DB, View, Keys) ->
    read_doc(DB, View, Keys, true).

read_doc(DB, View, Keys, Flag) ->
    Fun = fun(R, A) -> [document:read(<<"value">>, R) | A] end,
    case couchbeam_view:fold(Fun, [], DB, View, Keys) of
        {error, Reason} ->
            {error, Reason};
        [] when Flag == true ->
            {error, not_found};
        [] when Flag == false ->
            {ok, []};
        [Row] ->
            {ok, Row};
        Rows ->
            {ok, Rows}
    end.

delete_doc(DB, Id) ->
    case read_doc(DB, Id) of
        {ok, Doc} ->
            couchbeam:delete_doc(DB, Doc);
        {error, Error} ->
            {error, Error}
    end.

delete_doc(DB, View, Keys) ->
    Fun = fun(R, A) -> [document:read(<<"value">>, R) | A] end,
    case couchbeam_view:fold(Fun, [], DB, View, Keys) of
        {error, Reason} ->
            {error, Reason};
        [] ->
            {error, not_found};
        Rows ->
            {ok, [couchbeam:delete_doc(DB, Doc) || Doc <- Rows]}
    end.

exists(DB, Id) ->
    case read_doc(DB, Id) of
        {ok, _} ->
            true;
        {error, not_found} ->
            false;
        {error, Error} ->
            {error, Error}
    end.

exists(DB, View, Keys) ->
    case read_doc(DB, View, Keys) of
        {ok, _} ->
            true;
        {error, not_found} ->
            false;
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

set_attachments(Doc, [{F, C, B} | T]) ->
    Doc1 = couchbeam_attachments:add_inline(Doc, B, F, C),
    set_attachments(Doc1, T); 
set_attachments(Doc, []) ->
    Doc.

%% ###############################################################
%% ###############################################################
%% ############################################################### 

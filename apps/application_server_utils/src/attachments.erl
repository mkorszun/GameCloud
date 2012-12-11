%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Attachments
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(attachments).
-export([get/3]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(ATTACHMENT, <<"_attachments">>).
-define(EXCLUDE, [<<"revpos">>, <<"stub">>]).

%% ###############################################################
%%
%% ###############################################################

get(DB, Docs, Include) when is_list(Docs) ->
    lists:map(fun(Doc) -> get(DB, Doc, Include) end, Docs);

get(DB, {L} = Doc, Include) when is_list(L) ->
    case document:read(?ATTACHMENT, Doc) of
        {Attachments} ->
            Id = document:get_id(Doc),
            Fun = fun(A) -> attachments(DB, Id, A, Include) end,
            New = lists:map(Fun, Attachments),
            document:set_value(?ATTACHMENT, {New}, Doc);
        undefined ->
            Doc
    end;
get(_, Doc, _) ->
    Doc.
  
%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################
  
attachments(DB, Id, Attachment, Flag) when is_list(Flag) ->
    attachments(DB, Id, Attachment, list_to_atom(Flag));
attachments(DB, Id, Attachment, Flag) when is_binary(Flag) ->
    attachments(DB, Id, Attachment, binary_to_list(Flag));
attachments(_, _, {Name, Properties}, false) ->
    {Name, document:delete(Properties, ?EXCLUDE)};
attachments(DB, Id, {Name, Properties}, true) ->
    {ok, Content} = couchbeam:fetch_attachment(DB, Id, Name),
    Doc = document:delete(Properties, ?EXCLUDE),
    {Name, document:set_value(<<"content">>, base64:encode(Content), Doc)}.

%% ###############################################################
%% ###############################################################
%% ###############################################################

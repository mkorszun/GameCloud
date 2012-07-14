-module(create_api).

-export([out/1]).

out(A) ->
    case yaws_api:parse_multipart_post(A) of
	{cont, Cont, Res} ->
	    St = handle_res(A, Res, [], []),
	    {get_more, Cont, St};
        {result, Res} ->
            {P, F} = handle_res(A, Res, [], []),
	    Conn = couchbeam:server_connection(),
	    {ok, DB} = couchbeam:open_db(Conn, "save_cloud"),
	    Doc = set_attachments({P}, F),
	    {ok, _} = couchbeam:save_doc(DB, Doc),
            {html, "<h1>Done</h1>"};
        {error, _Reason} ->
            {html, "<h1>Error occured</h1>"}
    end.

set_attachments(Doc, [{F, C, B} | T]) ->
    set_attachments(couchbeam_attachments:add_inline(Doc, B, F, C), T);
set_attachments(Doc, []) ->
    Doc.

handle_res(A, [{head, {Name, [{"name", Name}]}}, {body, Value} | T], Props, Files) ->
    handle_res(A, T, [{Name, Value} | Props], Files);
handle_res(A, [{head, {Name, [{"name", Name}]}}, {part_body, Value} | T], Props, Files) ->
    handle_res(A, T, [{Name, Value} | Props], Files);
handle_res(A, [{head, {Name, [{"name", Name} | Z]}}, {body, Value} | T], Props, Files) ->
    ContentType = proplists:get_value(content_type, Z),
    handle_res(A, T, Props, [{Name, ContentType, Value} | Files]);
handle_res(A, [{head, {Name, [{"name", Name} | Z]}}, {part_body, Value} | T], Props, Files) ->
    ContentType = proplists:get_value(content_type, Z),
    handle_res(A, T, Props, [{Name, ContentType, Value} | Files]);
handle_res(_A, [], Props, Files) ->
    {Props, Files}.

-module(doc_api).
-export([out/1]).

out(_A) ->
	[{status, 200}, {content, "text/xml", "Doc"}].
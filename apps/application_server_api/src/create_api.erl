%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Save creation API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(create_api).
-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

-define(DB, couchdb_db).
-define(APP, application_server_api).

%% ###############################################################
%% CALLBACK FUNCTION
%% ############################################################### 

out(A) ->
    case yaws_api:parse_multipart_post(A) of
	{cont, Cont, Res} ->
	    St = multipart:handle_res(A, Res, [], []),
	    {get_more, Cont, [St | state(A)]};
        {result, Res} ->
            {P, F} = multipart:handle_res(A, Res, [], []),
	    {ok, DBName} = application:get_env(?APP, ?DB),
	    {ok, DB} = database:open(DBName),
	    User = proplists:get_value("user_id", P),
	    Pass = proplists:get_value("password", P),
	    create_save(DB, P, F, User, Pass);
        {error, _Reason} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

create_save(_, _, _, undefined, _) ->
    [{status, 404}, {content, "text/xml", "Missing user name"}];
create_save(_, _, _, _, undefined) ->
    [{status, 404}, {content, "text/xml", "Missing password"}];
create_save(_, _, _, [], _) ->
    [{status, 400}, {content, "text/xml", "Empty user name"}];
create_save(_, _, _, _, []) ->
    [{status, 400}, {content, "text/xml", "Empty password"}];

create_save(DB, P, F, User, Pass) ->
    case database:read_doc(DB, User) of
	{ok, UserDoc} ->
	    UserPass = document:read("password", UserDoc),
	    AuthRes = authorization:auth(UserPass, Pass),
	    create_save(DB, P, F, AuthRes);
	{error, not_found} ->
	    [{status, 404},{content, "text/xml", "User not found"}];
	{error, _Error} ->
	    [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

create_save(DB, P, F, true) ->
    Doc = document:create(P),
    {ok, Doc1} = database:save_doc(DB, Doc, F),
    DocId = document:get_id(Doc1),
    [{status, 200}, {content, "text/xml", DocId}];
create_save(_, _, _, false) ->
    [{status, 401}, {content, "text/xml", "Unauthorized"}].

state(#arg{state = undefined}) -> [];
state(#arg{state = State}) when is_list(State) -> State;
state(#arg{state = _}) -> [].
    
%% ###############################################################
%% ###############################################################
%% ############################################################### 

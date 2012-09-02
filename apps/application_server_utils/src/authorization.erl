%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Authorization functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(authorization).
-export([authorize1/3, authorize1/4, authorize2/5]).

%% ###############################################################
%%
%% ###############################################################

authorize1(User, DB, Params) ->
    authorize1(User, DB, Params, false).

authorize1(player, DB, Params, ReturnDoc) ->
    User = proplists:get_value("user_id", Params),
    Pass = proplists:get_value("user_pass", Params),
    authorize2(DB, User, Pass, "user_pass", ReturnDoc);

authorize1(developer, DB, Params, ReturnDoc) ->
    User = proplists:get_value("developer_id", Params),
    Pass = proplists:get_value("dev_pass", Params),
    authorize2(DB, User, Pass, "dev_pass", ReturnDoc).

authorize2(DB, User, Pass, PassKey, ReturnDoc) ->
    case database:read_doc(DB, User) of
        {ok, UserDoc} ->
            UserPass = document:read(PassKey, UserDoc),
            return(UserDoc, auth(UserPass, Pass), ReturnDoc);
        {error, Reason} ->
            {error, Reason}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

return(Doc, Result, true) ->
    {ok, Result, Doc};
return(_Doc, Result, false) ->
    {ok, Result}.

auth(P1, P2) when is_list(P1) ->
    auth(list_to_binary(P1), P2);
auth(P1, P2) when is_list(P2) ->
    auth(P1, list_to_binary(P2));

auth(P, P) -> true;
auth(_, _) -> false.

%% ###############################################################
%% ###############################################################
%% ###############################################################    

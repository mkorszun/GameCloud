%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud 
%%% @doc
%%% Register developer API
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(register_developer_api).
-export([out/1]).

%% ###############################################################
%% INCLUDES
%% ###############################################################

-include("api.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(TYPE, {"type", "developer"}).

%% ###############################################################
%% CALLBACK FUNCTION
%% ###############################################################

out(A) ->
    Args = yaws_api:parse_post(A),
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, DB} = database:open(DBName),
    Register = fun() -> register_developer(DB, Args) end,
    request:execute(validate(), Args, Register).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

register_developer(DB, Args) ->
    Id = proplists:get_value("developer_id", Args),
    Params = proplists:delete("developer_id", Args),
    Doc = document:create([{"_id", Id}, ?TYPE | Params]),
    case database:save_doc(DB, Doc) of
        {ok, _} ->
            [{status, 200}, {content, "text/xml", "ok"}];
        {error, conflict} ->
            [{status, 400}, {content, "text/xml", "Developer already exists"}];
        {error, _Error} ->
            [{status, 500}, {content, "text/xml", "Internal error"}]
    end.

%% ###############################################################
%% VALIDATE PARAMS
%% ###############################################################

validate() ->
    [
        {"developer_id", undefined, 404, "text/xml", "Missing developer id"},
        {"developer_id", [], 400, "text/xml", "Empty developer id"},
        {"dev_pass", undefined, 404, "text/xml", "Missing password"},
        {"dev_pass", [], 400, "text/xml", "Empty password"},
        {"email", undefined, 404, "text/xml", "Missing email"},
        {"email", [], 400, "text/xml", "Empty email"}
    ].

%% ###############################################################
%% ###############################################################
%% ###############################################################

%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Views for different requests and args
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(views).
-export([view/2]).

-include("types.hrl").

-define(DESIGN_DOC, <<"game_saves">>).
-define(USER_GAME, <<"by_user_game">>).
-define(USER_GAME_SAVE, <<"by_user_game_save">>).
-define(ID, <<"by_id">>).

%% ###############################################################
%% 
%% ###############################################################

-spec view(atom(), proplist() | document()) -> {view(), keys()}.

view(read_by_name, Args) ->
    case proplists:get_value("name", Args) of 
        undefined ->
            View = {?DESIGN_DOC, ?USER_GAME}, 
            Keys = view_keys(["user_id", "game"], Args),
            {View, Keys}; 
        _ -> 
            View = {?DESIGN_DOC, ?USER_GAME_SAVE},
            Keys = view_keys(["user_id", "game", "name"], Args), 
            {View, Keys}
    end;

view(read_by_id, Args) ->
    View = {?DESIGN_DOC, ?ID},
    Keys = view_keys(["id"], Args),
    {View, Keys};

view(delete_by_name, Args) ->
     case proplists:get_value("name", Args) of 
        undefined ->
            View = {?DESIGN_DOC, ?USER_GAME}, 
            Keys = view_keys(["user_id", "game"], Args),
            {View, Keys}; 
        _ -> 
            View = {?DESIGN_DOC, ?USER_GAME_SAVE},
            Keys = view_keys(["user_id", "game", "name"], Args), 
            {View, Keys}
    end;

view(delete_by_id, Args) ->
    View = {?DESIGN_DOC, ?ID},
    Keys = {key, list_to_binary(proplists:get_value("id", Args))},
    {View, Keys};        

view(create, Args) ->
    View = {?DESIGN_DOC, ?USER_GAME_SAVE},
    Keys = view_keys(["user_id", "game", "name"], Args), 
    {View, Keys}.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

view_keys(Keys) -> 
    {key, keys(Keys)}.

view_keys(Names, L) when is_list(L) -> 
    view_keys([proplists:get_value(N, L) || N <- Names]); 
view_keys(Names, Doc)  ->
    view_keys([document:read(K, Doc) || K <- Names]).

keys([]) -> [];
keys([H|T]) when is_binary(H) -> [H | keys(T)];
keys([H|T]) when is_list(H) -> [list_to_binary(H) | keys(T)].

%% ###############################################################
%% ###############################################################
%% ###############################################################

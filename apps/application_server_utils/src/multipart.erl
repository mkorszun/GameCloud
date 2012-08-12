%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% YAWS Multipart helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(multipart).
-export([handle_res/3, handle_res/4, build_params/4, state/1]).

%% ###############################################################
%% INCLUDES
%% ############################################################### 

-include_lib("yaws/include/yaws_api.hrl"). 

%% ###############################################################
%%       
%% ############################################################### 

handle_res(A, [{head, {Name, [{"name", Name} | Z]}} | T], ACC) ->
    handle_res(A, T, [{Name, undefined, Z} | ACC]);
handle_res(A, [{part_body, Value} | T], [{Name, undefined, Z} | ACCT]) ->
    handle_res(A, T, [{Name, {part_body, Value}, Z} | ACCT]);
handle_res(A, [{part_body, Value} | T], [{Name, {part_body, Body, Z}} | ACCT]) ->
    handle_res(A, T, [{Name, {part_body, lists:append(Body, Value)}, Z} | ACCT]);
handle_res(A, [{body, Value} | T], [{Name, undefined, Z} | ACCT]) ->
    handle_res(A, T, [{Name, Value, Z} | ACCT]);
handle_res(A, [{body, Value} | T], [{Name, {part_body, Body}, Z} | ACCT]) ->
    handle_res(A, T, [{Name, lists:append(Body, Value), Z} | ACCT]);
handle_res(_A, [], ACC) ->
    ACC.

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

build_params(A, [{Name, Value, []} | T], Props, Files) ->
    build_params(A, T, [{Name, lists:flatten(Value)} | Props], Files);
build_params(A, [{Name, Value, Params} | T], Props, Files) ->
    ContentType = proplists:get_value(content_type, Params),
    build_params(A, T, Props, [{Name, ContentType, lists:flatten(Value)} | Files]);
build_params(_A, [], Props, Files) ->
    {Props, Files}.

state(#arg{state = undefined}) -> [];
state(#arg{state = State}) when is_list(State) -> State; 
state(#arg{state = _}) -> [].

%% ###############################################################
%% ###############################################################
%% ############################################################### 

%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Request execution mechanism
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(request).
-export([execute/3, get_method/1]).

%% ###############################################################
%% INCLUDES
%% ############################################################### 

-include_lib("yaws/include/yaws_api.hrl"). 

%% ###############################################################
%%
%% ###############################################################

execute([], _Params, Logic) -> Logic();
execute([{Name, Pattern, Code, Response} | T], Params, Logic) ->
    case proplists:get_value(Name, Params) of
        Pattern -> {error, {missing_param, Code, Response}};
        Value when is_list(Value) -> execute(T, Params, Logic)
    end.
 
get_method(#arg{req = #http_request{method = Method}}) ->
	Method.

%% ###############################################################
%% ###############################################################
%% ###############################################################

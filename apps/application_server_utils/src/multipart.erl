%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% YAWS Multipart helper functions
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(multipart).
-export([handle_res/4]).

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

%% ###############################################################
%% ###############################################################
%% ############################################################### 

%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Utils
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_api_utils).

-export([request_body/1, set_location/2, build_page/1, decode_list/1]).

%% ###############################################################
%% UTILS
%% ###############################################################

request_body(ReqData) ->
    mochijson2:decode(wrq:req_body(ReqData)).

set_location(Id, ReqData) when is_binary(Id) ->
    set_location(binary_to_list(Id), ReqData);
set_location(Id, ReqData) when is_list(Id) ->
    BaseURI = wrq:base_uri(ReqData),
    Path = wrq:path(ReqData),
    NewPath = case [lists:last(Path)] of
        "/" -> Path;
        _ -> Path ++ "/"
    end,
    Resource = string:join([BaseURI, NewPath, Id], ""),
    wrq:set_resp_header("Location", Resource, ReqData).

build_page(Games) ->
    List = lists:map(fun(Game) -> build_link(Game) end, Games),
    {ok, Page} = application:get_env(game_cloud_api, page),
    io_lib:format(Page, [List]).

decode_list(List) when is_list(List) ->
    Left = string:left(List, 1),
    Right = string:right(List, 1),
    case Left == "[" andalso Right == "]" of
        true ->
            Elements = string:sub_string(List, 2, length(List) - 1),
            [list_to_binary(string:strip(X)) || X <- string:tokens(Elements , ",")];
        false ->
            [list_to_binary(List)]
    end;
decode_list(_) -> [].

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_link(Game) ->
    MarketLink = document:read(<<"market_link">>, Game),
    {ok, Link} = application:get_env(game_cloud_api, link),
    io_lib:format(Link, [MarketLink, game:path(screen, Game)]).

%% ###############################################################
%% ###############################################################
%% ###############################################################
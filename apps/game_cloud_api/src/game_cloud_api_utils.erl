%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Utils
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_api_utils).

-compile([{parse_transform, lager_transform}]).

-export([request_body/1, set_location/2, is_authorized/3, build_page/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

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

is_authorized(developer, ReqData, Context) ->
    authorize(ReqData, Context,
        fun(Data, Ctx, User, Pass) ->
            case developer:authorize(User, Pass) of
                true ->
                    {true, Data, [{user, User}]};
                false ->
                    ?ERR("Developer id=~s not authorized",
                        [User]),
                    {"Basic realm=GameCloud", Data, Ctx};
                {error, not_found} ->
                    ?ERR("Failed to authorize developer id=~s: not_found",
                        [User]),
                    {{halt, 404}, Data, Ctx};
                {error, Error} ->
                    ?ERR("Failed to authorize developer id=~s: ~p",
                        [User, Error]),
                    {{halt, 500}, Data, Ctx}
            end
        end
    ).

build_page(DeveloperId, Games) ->
    List = lists:map(fun({Game}) -> build_link(DeveloperId, Game) end, Games),
    io_lib:format("<html><body><ul>~s</ul></body></html>", [List]).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

build_link(DeveloperId, Game) ->
    {ok, Host} = application:get_env(game_cloud_api, server_addr),
    {ok, Port} = application:get_env(game_cloud_api, server_port),
    GameKey = proplists:get_value(<<"key">>, Game),
    MarketLink = proplists:get_value(<<"market_link">>, Game),
    {Screen} = proplists:get_value(<<"screen">>, Game),
    ScreenName = proplists:get_value(<<"name">>, Screen),
    ImageLink = io_lib:format("http://~s:~p/developer/~s/game/~s/screen/~s",
        [inet_parse:ntoa(Host), Port, DeveloperId, GameKey, ScreenName]),
    io_lib:format("<li><a href=\"?argument=~s\"><img src=\"~s\"/></a></li>",
        [MarketLink, ImageLink]).

authorize(ReqData, Context, Fun) ->
    case wrq:get_req_header("authorization", ReqData) of
        "Basic "++Base64 ->
            Str = base64:mime_decode_to_string(Base64),
            case string:tokens(Str, ":") of
                [User, Pass] ->
                    Fun(ReqData, Context, User, Pass);
                _ ->
                    {"Basic realm=GameCloud", ReqData, Context}
            end;
        _ ->
            {"Basic realm=GameCloud", ReqData, Context}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Authorization
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_api_auth).

-compile([{parse_transform, lager_transform}]).

-export([is_authorized/3, is_authorized/4]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%% AUTHORIZATION UTILS
%% ###############################################################

is_authorized(developer, ReqData, Context) ->
    is_authorized(developer, ReqData, Context, both).

is_authorized(developer, ReqData, Context, Method) ->
    authorize(ReqData, Context, Method,
        fun(basic, Data, Ctx, Attributes) ->
            User = proplists:get_value(user, Attributes),
            Pass = proplists:get_value(pass, Attributes),
            case developer:authorize(User, Pass) of
                true ->
                    {true, Data, [{user, User} | Ctx]};
                false ->
                    ?ERR("Developer id=~s not authorized",
                        [User]),
                    {{halt, 401}, Data, Ctx};
                {error, not_found} ->
                    ?ERR("Failed to authorize developer id=~s: not_found",
                        [User]),
                    {{halt, 404}, Data, Ctx};
                {error, Error} ->
                    ?ERR("Failed to authorize developer id=~s: ~p",
                        [User, Error]),
                    {{halt, 500}, Data, Ctx}
            end;
        (token, Data, Ctx, Attributes) ->
            Path = wrq:path_info(ReqData),
            DeveloperId = dict:fetch(developer, Path),
            Token = proplists:get_value(token, Attributes),
            case token:check_token(DeveloperId, Token) of
                {ok, _} ->
                    {true, Data, [{user, DeveloperId} | Ctx]};
                {error, token_expired} ->
                    ?ERR("Failed to authorize developer id=~s: token expired",
                        [DeveloperId]),
                    {{halt, 401}, Data, Ctx};
                {error, not_found} ->
                    ?ERR("Failed to authorize developer id=~s: token missing",
                        [DeveloperId]),
                    {false, Data, Ctx};
                {error, Error} ->
                    ?ERR("Failed to authorize developer id=~s: ~p",
                        [DeveloperId, Error]),
                    {{halt, 500}, Data, Ctx}
            end
        end
    ).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

authorize(ReqData, Context, Method, Fun) ->
    case wrq:get_req_header("authorization", ReqData) of
        "gc_auth_token "++Token when Method == both orelse Method == token ->
            Fun(token, ReqData, Context, [{token, Token}]);
        "Basic "++Base64 when Method == both orelse Method == basic ->
            case string:tokens(base64:mime_decode_to_string(Base64), ":") of
                [User, Pass] ->
                    Fun(basic, ReqData, Context, [{user, User}, {pass, Pass}]);
                _ ->
                    {"Basic realm=GameCloud", ReqData, Context}
            end;
        _ ->
            {"Basic realm=GameCloud", ReqData, Context}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
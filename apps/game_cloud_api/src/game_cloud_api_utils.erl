%%% @author Mateusz Korszun <mkorszun@gmail.com>
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Utils
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(game_cloud_api_utils).

-compile([{parse_transform, lager_transform}]).

-export([get_request_body/3, is_authorized/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("logger.hrl").

%% ###############################################################
%%
%% ###############################################################

get_request_body(ReqData, State, Key) ->
    case proplists:get_value(Key, State) of
        undefined ->
            Body = wrq:req_body(ReqData),
            {struct, Object} = mochijson2:decode(Body),
            {Object, [{Key, Object} | State]};
        Else ->
            {Else, State}
    end.

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
                    ?ERR("Failed to authorize developer id=~s: ~s",
                        [User, Error]),
                    {{halt, 500}, Data, Ctx}
            end
        end
    ).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

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
%%
%% ###############################################################
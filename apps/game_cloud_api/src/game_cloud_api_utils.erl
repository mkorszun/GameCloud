-module(game_cloud_api_utils).

-compile([{parse_transform, lager_transform}]).

-export([get_request_body/3, authorize/3, is_authorized/3]).

-include("logger.hrl").

get_request_body(ReqData, undefined, Key) ->
	Body = wrq:req_body(ReqData),
	{struct, Developer} = mochijson2:decode(Body),
	{Developer, [{Key, Developer}]};
get_request_body(ReqData, State, Key) when is_list(State) ->
	case proplists:get_value(Key, State) of
		undefined ->
			get_request_body(ReqData, undefined, Key);
		Else ->
			{Else, State}
	end.

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
	);

is_authorized(player, ReqData, Context) ->
    authorize(ReqData, Context,
		fun(Data, Ctx, User, Pass) ->
            GameId = dict:fetch(game, wrq:path_info(Data)),
	        case player:authorize(GameId, User, Pass) of
	    		true ->
	        		{true, Data, [{user, User}]};
	        	false ->
	        		?ERR("Player id=~s not authorized", 
	        			[User]),
	        		{"Basic realm=GameCloud", Data, Ctx};
	        	{error, not_found} ->
	        		?ERR("Failed to authorize player id=~s: not_found", 
	        			[User]),
	        		{{halt, 404}, Data, Ctx};
	        	{error, Error} ->
	        		?ERR("Failed to authorize player id=~s: ~s", 
	        			[User, Error]),
	        		{{halt, 500}, Data, Ctx}
	        end
	    end
	).	
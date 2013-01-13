%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the game_cloud_api application.

-module(game_cloud_api_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for game_cloud_api.
start(_Type, _StartArgs) ->
    game_cloud_api_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for game_cloud_api.
stop(_State) ->
    ok.

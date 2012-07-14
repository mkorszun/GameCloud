-module(application_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = application_server_sup:start_link(),
    application_server_http:start_http_server(application_server_sup),
    Res.

stop(_State) ->
    ok.

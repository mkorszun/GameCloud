%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, SaveCloud
%%% @doc
%%% Application
%%% @end                       
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(application_server_utils_app).
-behaviour(application).

-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS       
%% ############################################################### 

start(_StartType, _StartArgs) ->
    application_server_utils_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ############################################################### 

%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Embedded YAWS
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(application_server_http).
-export([start_http_server/1]).

%% ###############################################################
%% MACROS
%% ############################################################### 

-define(APP, application_server).
-define(ID, http_server).

%% ###############################################################
%% SERVER START FUNCTION
%% ############################################################### 

start_http_server(Supervisor) ->

    {ok, Addr} = application:get_env(?APP, server_addr),
    {ok, Port} = application:get_env(?APP, server_port),
    {ok, Mods} = application:get_env(?APP, server_mods),

    Docroot = path_to_priv(?APP, ["www"]),   
    EbinDir = filename:join([Docroot, "..", "..", "ebin"]),

    GconfList = [{ebin_dir, [EbinDir]}, {id, ?ID}],    
    SconfList = [{docroot, Docroot}, {port, Port}, {listen, Addr}, 
                 {errormod_crash, yaws_error_reporter}, {appmods, Mods}],
          
    {ok, SCList, GC, ChildSpecs} =  
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, ?ID),
    [supervisor:start_child(Supervisor, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList). 

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ############################################################### 

path_to_priv(App, Path) ->
    filename:join([code:priv_dir(App) | Path]).

%% ###############################################################
%% ###############################################################
%% ############################################################### 

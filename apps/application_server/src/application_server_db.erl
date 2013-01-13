%%% @author Mateusz Korszun <mkorszun@gmail.com> 
%%% @copyright (C) 2012, GameCloud
%%% @doc
%%% Database connection
%%% @end
%%% Created : 20 Jun 2012 by Mateusz Korszun <mkorszun@gmail.com>

-module(application_server_db).

-export([connection/0]).

-define(APP, application_server).

connection() ->
    {ok, Name} = application:get_env(?APP, couchdb_db),
    {ok, Address} = application:get_env(?APP, couchdb_addr),
    {ok, Port} = application:get_env(?APP, couchdb_port),
    {ok, DB} = database:open(Name, inet_parse:ntoa(Address), Port),
    DB.
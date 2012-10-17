-module(application_server_db).

-export([connection/0]).

-include("db.hrl").

connection() ->
    {ok, DBName} = application:get_env(?APP, ?DB),
    {ok, Address} = application:get_env(?APP, ?DB_ADDR),
    {ok, DBPort} = application:get_env(?APP, ?DB_PORT),
    {ok, DB} = database:open(DBName, addr(Address), DBPort),
    DB.

addr(Addr) ->
	string:join([integer_to_list(X)||X<-erlang:tuple_to_list(Addr)], ".").
-module(application_server_db).

-export([connection/0]).

-define(APP, application_server).

connection() ->
    {ok, DBName} = application:get_env(?APP, couchdb_db),
    {ok, Address} = application:get_env(?APP, couchdb_addr),
    {ok, DBPort} = application:get_env(?APP, couchdb_port),
    {ok, DB} = database:open(DBName, addr(Address), DBPort),
    DB.

addr(Addr) ->
	string:join([integer_to_list(X)||X<-erlang:tuple_to_list(Addr)], ".").
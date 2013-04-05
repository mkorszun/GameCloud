-module(game_session_stats).

-export([save/4, read/3]).

save(GameKey, session_time, SessionId, Time) ->
    Object = riakc_obj:new(GameKey, SessionId, Time),
    Connection = pooler:take_member(stats),
    riakc_pb_socket:put(Connection, Object),
    pooler:return_member(stats, Connection, ok).

read(GameKey, session_time, SessionId) ->
    Connection = pooler:take_member(stats),
    {ok, Obj} = riakc_pb_socket:get(Connection, GameKey, SessionId),
    pooler:return_member(stats, Connection, ok),
    riakc_obj:get_value(Obj).
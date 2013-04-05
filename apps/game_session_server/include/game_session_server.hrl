-define(I2B(Int), list_to_binary(integer_to_list(Int))).
-define(L2A(L), try list_to_existing_atom(L) of A -> A catch _:_ -> list_to_atom(L) end).
-define(B2A(Bin), ?L2A(binary_to_list(Bin))).

-define(ERR(Format, Params), lager:error(Format, Params)).
-define(INF(Format, Params), lager:info(Format, Params)).
-define(DBG(Format, Params), lager:debug(Format, Params)).

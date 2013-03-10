-define(DEVELOPER_SCHEMA,
    {struct,[{<<"description">>,<<"A developer">>},
         {<<"type">>,<<"object">>},
         {<<"properties">>, {struct,[
                    {<<"id">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"email">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"password">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}}
            ]}},
         {<<"additionalProperties">>, false}]}).

-define(GAME_SCREEN,
    {struct,[{<<"description">>,<<"A screen">>},
         {<<"type">>,<<"object">>},
         {<<"properties">>, {struct,[
                    {<<"name">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"content_type">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"content">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}}
            ]}},
         {<<"additionalProperties">>, false},
         {<<"required">>,true}]}).

-define(GAME_SCHEMA,
    {struct,[{<<"description">>,<<"A game">>},
         {<<"type">>,<<"object">>},
         {<<"properties">>, {struct,[
                    {<<"developer_id">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"name">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"description">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"platform">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}, {<<"enum">>, [<<"ios">>, <<"android">>]}]}},
                    {<<"game_link">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"market_link">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}]}},
                    {<<"tags">>,{struct,[{<<"type">>,<<"array">>},{<<"required">>,true}, {<<"items">>, {struct, [{<<"type">>, <<"string">>}]}}]}},
                    {<<"status">>,{struct,[{<<"type">>,<<"string">>},{<<"required">>,true}, {<<"enum">>, [<<"new">>, <<"beta">>, <<"published">>]}]}},
                    {<<"screen">>,?GAME_SCREEN}
            ]}},
         {<<"additionalProperties">>, false}]}).

check(Model, Data) ->
    case jesse:validate_with_schema(Model, Data) of
        {ok, Data} ->
            {ok, Data};
        {error, {data_invalid, _, Reason, _}} ->
            throw(Reason);
        _ ->
            throw(bad_data)
    end.
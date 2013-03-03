-define(DEVELOPER_SCHEMA,
    {struct,[{<<"description">>,<<"A developer">>},
         {<<"type">>,<<"object">>},
         {<<"properties">>,
          {struct,[{<<"id">>,
                    {struct,[{<<"type">>,<<"string">>},
                             {<<"required">>,true}]}},
                   {<<"email">>,
                    {struct,[{<<"type">>,<<"string">>},
                             {<<"required">>,true}]}},
                   {<<"password">>,
                    {struct,[{<<"type">>,<<"string">>},
                             {<<"required">>,true}]}}]}},
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
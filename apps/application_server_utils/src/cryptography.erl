-module(cryptography).

-export([sha/2]).

sha(Data, Salt) ->
    C1 = crypto:sha_init(),
    C2 = crypto:sha_update(C1, Data),
    C3 = crypto:sha_update(C2, Salt),
    bin_to_hexstr(crypto:sha_final(C3)).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).
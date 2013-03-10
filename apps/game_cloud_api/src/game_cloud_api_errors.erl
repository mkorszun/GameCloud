-module(game_cloud_api_errors).

-export([error/1]).

error(not_object) ->
    mochijson2:encode({[{error, <<"Not an object">>}]});
error(no_extra_properties_allowed) ->
    mochijson2:encode({[{error, <<"No extra properties allowed">>}]});
error(missing_required_property) ->
    mochijson2:encode({[{error, <<"Missing required property">>}]});
error(not_string) ->
    mochijson2:encode({[{error, <<"Not a string">>}]});
error(not_in_enum) ->
    mochijson2:encode({[{error, <<"Not in enum">>}]});

error(_) -> [].

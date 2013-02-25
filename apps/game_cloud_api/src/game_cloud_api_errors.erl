-module(game_cloud_api_errors).

-export([error/1]).

error(empty_doc) ->
    mochijson2:encode({[{error, <<"Empty document">>}]});

error(wrong_id_format) ->
    mochijson2:encode({[{error, <<"Wrong id format">>}]});
error(wrong_email_format) ->
    mochijson2:encode({[{error, <<"Wrong email format">>}]});
error(wrong_password_format) ->
    mochijson2:encode({[{error, <<"Wrong password format">>}]});
error(missing_id) ->
    mochijson2:encode({[{error, <<"Missing id">>}]});
error(missing_email) ->
    mochijson2:encode({[{error, <<"Missing email">>}]});
error(missing_password) ->
    mochijson2:encode({[{error, <<"Missing password">>}]});

error(wrong_developer_id_format) ->
    mochijson2:encode({[{error, <<"Wrong developer id format">>}]});
error(wrong_name_format) ->
    mochijson2:encode({[{error, <<"Wrong name format">>}]});
error(wrong_description_format) ->
    mochijson2:encode({[{error, <<"Wrong description format">>}]});
error(wrong_platform_format) ->
    mochijson2:encode({[{error, <<"Wrong platform format">>}]});
error(wrong_game_link_format) ->
    mochijson2:encode({[{error, <<"Wrong game link format">>}]});
error(wrong_market_link_format) ->
    mochijson2:encode({[{error, <<"Wrong market link format">>}]});
error(wrong_tags_format) ->
    mochijson2:encode({[{error, <<"Wrong tags format">>}]});
error(wrong_status_format) ->
    mochijson2:encode({[{error, <<"Wrong status format">>}]});
error(wrong_screen_format) ->
    mochijson2:encode({[{error, <<"Wrong screen format">>}]});
error(wrong_screen_name_format) ->
    mochijson2:encode({[{error, <<"Wrong screen name format">>}]});
error(wrong_screen_content_type_format) ->
    mochijson2:encode({[{error, <<"Wrong screen content type format">>}]});
error(wrong_screen_content_format) ->
    mochijson2:encode({[{error, <<"Wrong screen content format">>}]});
error(missing_developer_id) ->
    mochijson2:encode({[{error, <<"Missing developer id">>}]});
error(missing_name) ->
    mochijson2:encode({[{error, <<"Missing name">>}]});
error(missing_description) ->
    mochijson2:encode({[{error, <<"Missing description">>}]});
error(missing_platform) ->
    mochijson2:encode({[{error, <<"Missing platform">>}]});
error(missing_game_link) ->
    mochijson2:encode({[{error, <<"Missing game link">>}]});
error(missing_market_link) ->
    mochijson2:encode({[{error, <<"Missing market link">>}]});
error(missing_tags) ->
    mochijson2:encode({[{error, <<"Missing tags">>}]});
error(missing_status) ->
    mochijson2:encode({[{error, <<"Missing status">>}]});
error(missing_screen) ->
    mochijson2:encode({[{error, <<"Missing screen">>}]});
error(missing_screen_name) ->
    mochijson2:encode({[{error, <<"Missing screen name">>}]});
error(missing_screen_content_type) ->
    mochijson2:encode({[{error, <<"Missing screen content type">>}]});
error(missing_screen_content) ->
    mochijson2:encode({[{error, <<"Missing screen content">>}]});

error(_) -> [].

-module(application_server_utils).

-compile(export_all).

path_to_priv(App, Path) ->
    filename:join([code:priv_dir(App) | Path]).

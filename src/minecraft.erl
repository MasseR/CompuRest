%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc minecraft.

-module(minecraft).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the minecraft server.
start() ->
    minecraft_deps:ensure(),
    ensure_started(crypto),
    application:start(minecraft).


%% @spec stop() -> ok
%% @doc Stop the minecraft server.
stop() ->
    application:stop(minecraft).

%% @author Mochi Media <dev@mochimedia.com>
%% @copyright minecraft Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the minecraft application.

-module(minecraft_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for minecraft.
start(_Type, _StartArgs) ->
    minecraft_deps:ensure(),
    itemstats_sup:start_link(),
    iteminfo_sup:start_link(),
    minecraft_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for minecraft.
stop(_State) ->
    ok.

%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for minecraft.

-module(minecraft_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    Path = Req:get(path),
    Method = Req:get(method),
    mc_response(Method, Path, Req).

mc_response('GET', "/itemstats/get/" ++ Key, Req) ->
    Req:ok({"text/plain", integer_to_list(itemstats_srv:value(Key))});
mc_response('PUT', "/itemstats/set/" ++ Key, Req) ->
    Count = list_to_integer(binary_to_list(Req:recv_body())),
    itemstats_srv:set(Key, Count),
    Req:ok({"text/plain", "ok"});
mc_response('PUT', "/itemstats/inc/" ++ Key, Req) ->
    Count = list_to_integer(binary_to_list(Req:recv_body())),
    itemstats_srv:increment(Key, Count),
    Req:ok({"text/plain", "ok"});
mc_response('PUT', "/itemstats/dec/" ++ Key, Req) ->
    Count = list_to_integer(binary_to_list(Req:recv_body())),
    itemstats_srv:inc(Key, Count),
    Req:ok({"text/plain", "ok"});
mc_response(_Method, _Path, Req) ->
    Req:not_found().

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.

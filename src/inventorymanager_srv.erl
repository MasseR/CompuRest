-module(inventorymanager_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, get_turtles/2, turtle_item/1, turtle_max/1, change_max/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {max}).

%%%===================================================================
%%% API
%%%===================================================================

%%---------
%% @doc
%% Return a list of turtles and item counts
%%
%% @end
%%--------
get_turtles(ItemId, Count) ->
    gen_server:call(?MODULE, {get, ItemId, Count}).

turtle_item(TurtleId) ->
    gen_server:call(?MODULE, {turtle_item, TurtleId}).

turtle_max(TurtleId) ->
    gen_server:call(?MODULE, {turtle_max, TurtleId}).

change_max(Max) ->
    gen_server:cast(?MODULE, {new_max, Max}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Max) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Max], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Max]) ->
    {ok, #state{max=Max}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, ItemId, Count}, _From, State) ->
    Turtles = itemstats_srv:owners(ItemId),
    Used = case itemstats_srv:value(ItemId) of
               X when X < Count -> Turtles;
               _ -> consume(Turtles, Count)
           end,
    lists:foreach(fun({TurtleId, UsedCount}) -> itemstats_srv:decrement(ItemId, TurtleId, UsedCount) end, Used),
    {reply, Used, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({new_max, Max}, State) ->
    {noreply, State#state{max=Max}};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end %%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

consume(Turtles, Count) ->
    consume(Turtles, Count, []).

consume(_Turtles, 0, Acc) -> Acc;
consume([{TurtleId, Count}|Xs], Left, Acc) ->
    case Left < Count of
        true ->
            consume(Xs, 0, [{TurtleId, Left} | Acc]);
        false ->
            consume(Xs, Left - Count, [{TurtleId, Count} | Acc])
    end.

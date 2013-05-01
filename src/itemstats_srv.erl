-module(itemstats_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, value/1, set/2, increment/2, decrement/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

value(Key) ->
    io:format("Trying to lookup (root) ~p~n", [Key]),
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:cast(?MODULE, {put, Key, Value}).

increment(Key, Amount) ->
    gen_server:cast(?MODULE, {inc, Key, Amount}).

decrement(Key, Amount) ->
    increment(Key, -Amount).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:format("Starting gen_server"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    dets_open("itemstats"),
    {ok, #state{}}.

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
handle_call({get, Key}, _From, State) ->
    io:format("Trying to lookup ~p~n", [Key]),
    {reply, internal_value(Key), State};
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
handle_cast({put, Key, Value}, State) ->
    dets:insert(?MODULE, {Key, Value}),
    {noreply, State};
handle_cast({inc, Key, Value}, State) ->
    OldValue = internal_value(Key),
    io:format("Incrementing ~p + ~p~n", [OldValue, Value]),
    NewValue = case OldValue + Value of
        X when X < 0 -> 0;
        X -> X
    end,
    dets:insert(?MODULE, {Key, NewValue}),
    {noreply, State};
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
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    dets_close(),
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

dets_open(Filename) ->
    Bool = filelib:is_file(Filename),
    case dets:open_file(?MODULE, {file, Filename}) of
        {ok, ?MODULE} ->
            case Bool of
                true -> void;
                false -> ok = dets:insert(?MODULE, {free, 1})
            end,
            true
    end.

dets_close() ->
    dets:close(?MODULE).

internal_value(Key) ->
    case dets:lookup(?MODULE, Key) of
        [] -> 0;
        [ {Key, X} ] -> X;
        Res ->
            io:format("~p~n", [Res]),
            0
    end.


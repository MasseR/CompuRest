-module(statistics_srv).

-behaviour(gen_server).

-export([start_link/0, uptime/0, complete_requests/0, writes/0, reads/0, write/0, read/0, record_request/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {started, requests=0, reads=0, writes=0}).

%% API

write() ->
    gen_server:cast(?MODULE, {write}).

read() ->
    gen_server:cast(?MODULE, {read}).

record_request() ->
    gen_server:cast(?MODULE, {request}).

uptime() ->
    gen_server:call(?MODULE, {uptime}).

complete_requests() ->
    gen_server:call(?MODULE, {complete_requests}).

writes() ->
    gen_server:call(?MODULE, {writes}).

reads() ->
    gen_server:call(?MODULE, {reads}).

%% gen_server API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{started=calendar:universal_time()}}.

handle_call({uptime}, _From, State) ->
    {reply, calendar:time_difference(State#state.started, calendar:universal_time()), State};
handle_call({complete_requests}, _From, State) ->
    {reply, State#state.requests, State};
handle_call({reads}, _From, State) ->
    {reply, State#state.reads, State};
handle_call({writes}, _From, State) ->
    {reply, State#state.writes, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({write}, State) ->
    #state{writes=Writes} = State, Writes,
    {noreply, State#state{writes = Writes+1}};
handle_cast({read}, State) ->
    #state{reads=Reads} = State, Reads,
    {noreply, State#state{reads = Reads+1}};
handle_cast({request}, State) ->
    #state{requests=Req} = State, Req,
    {noreply, State#state{requests = Req+1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldSvn, State, _Extra) ->
    {ok, State}.

%% Internal API


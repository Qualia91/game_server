%%%-----------------------------------------------------------------------------
%%% @doc
%%% Contains all functionality for collecting telemetry on the game server.
%%% Makes use of erlang counters to save telemetry. 
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_telemetry).
-author(boc_dev).
-behaviour(gen_server).

-include("game_server.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/0,
    game_started/0,
    game_ended/0,
    get_games_running/0,
    server_busy/0,
    get_server_busy/0,
    get_games_played/0,
    get_server_version/0,
    get_server_start_datetime/0,
    public_lobby_created/0,
    get_public_lobbies_created/0,
    private_lobby_created/0,
    get_private_lobbies_created/0,
    get_number_of_processes/0,
    websocket_connections/0,
    get_websocket_connections/0
]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

-define(SERVER, ?MODULE).

-define(GAMES_RUNNING_INDEX, 1).
-define(SERVER_BUSY_INDEX, 2).
-define(GAMES_PLAYED, 3).
-define(PUBLIC_LOBBIES_CREATED, 4).
-define(PRIVATE_LOBBIES_CREATED, 5).
-define(WEBSOCKET_CONNECTIONS, 6).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Loop state
-record(loop_state, {
    counter_obj           = counters:new(6, [write_concurrency]),
    server_start_datetime
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Call when creating a game to add one to games playes, and games running 
%% counters.
%% @end
%%------------------------------------------------------------------------------
-spec game_started() -> ok.
game_started() ->
    gen_server:cast(?SERVER, {telemetry, add, ?GAMES_PLAYED}),
    gen_server:cast(?SERVER, {telemetry, add, ?GAMES_RUNNING_INDEX}).

-spec game_ended() -> ok.
game_ended() ->
    gen_server:cast(?SERVER, {telemetry, sub, ?GAMES_RUNNING_INDEX}).

-spec get_games_running() -> integer().
get_games_running() ->
    gen_server:call(?SERVER, {telemetry, get, ?GAMES_RUNNING_INDEX}).

-spec server_busy() -> ok.
server_busy() ->
    gen_server:cast(?SERVER, {telemetry, add, ?SERVER_BUSY_INDEX}).

-spec get_server_busy() -> integer().
get_server_busy() ->
    gen_server:call(?SERVER, {telemetry, get, ?SERVER_BUSY_INDEX}).

-spec public_lobby_created() -> ok.
public_lobby_created() ->
    gen_server:cast(?SERVER, {telemetry, add, ?PUBLIC_LOBBIES_CREATED}).

-spec get_public_lobbies_created() -> integer().
get_public_lobbies_created() ->
    gen_server:call(?SERVER, {telemetry, get, ?PUBLIC_LOBBIES_CREATED}).

-spec private_lobby_created() -> ok.
private_lobby_created() ->
    gen_server:cast(?SERVER, {telemetry, add, ?PRIVATE_LOBBIES_CREATED}).

-spec get_private_lobbies_created() -> integer().
get_private_lobbies_created() ->
    gen_server:call(?SERVER, {telemetry, get, ?PRIVATE_LOBBIES_CREATED}).
    
-spec get_games_played() -> integer().
get_games_played() ->
    gen_server:call(?SERVER, {telemetry, get, ?GAMES_PLAYED}).
    
-spec get_server_start_datetime() -> list().
get_server_start_datetime() ->
    gen_server:call(?SERVER, {stats, server_start_datetime}).
    
-spec get_server_version() -> integer().
get_server_version() ->
    {ok, ServerVersion} = application:get_env(game_server, server_version),
    ServerVersion.
   
-spec get_number_of_processes() -> integer(). 
get_number_of_processes() ->
    length(registered()).

-spec websocket_connections() -> ok.
websocket_connections() ->
    gen_server:cast(?SERVER, {telemetry, add, ?WEBSOCKET_CONNECTIONS}).

-spec get_websocket_connections() -> integer().
get_websocket_connections() ->
    gen_server:call(?SERVER, {telemetry, get, ?WEBSOCKET_CONNECTIONS}).
    
%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([]) -> {ok, loop_state()}.
init([]) ->
    {{Y,M,D},{H,MM,SS}} = calendar:now_to_datetime(erlang:timestamp()),
    DateTimeStr = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D,H,MM,SS])),
    {ok, #loop_state{server_start_datetime = DateTimeStr}}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call({telemetry, Operation, Index}, _From, LoopState = #loop_state{counter_obj = CounterObj}) ->
    {reply, counters:Operation(CounterObj, Index), LoopState};
handle_call({stats, server_start_datetime}, _From, LoopState) ->
    {reply, LoopState#loop_state.server_start_datetime, LoopState};
handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {ok, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast({telemetry, Operation, Index}, LoopState = #loop_state{counter_obj = CounterObj}) ->
    counters:Operation(CounterObj, Index, 1),
    {noreply, LoopState};
handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
    ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

counters_test() ->

    start_link(),

    ?assertMatch(0, get_games_running(), <<"0 games running">>),

    game_started(),

    ?assertMatch(1, get_games_running(), <<"1 games running">>),
    
    game_ended(),

    ?assertMatch(0, get_games_running(), <<"0 games running">>),

    server_busy(),

    ?assertMatch(1, get_server_busy(), <<"1 server busy request sent">>).

-endif.
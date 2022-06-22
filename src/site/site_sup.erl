%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level site supervisor to create cowboy websocket endpoints for the stats
%%% and game.
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(site_sup).
-author(boc_dev).
-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/stats_handler", stats_handler, [{stats_interval, 10000}]},
            {"/game_handler", game_handler, [{stats_interval, 10000}]}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, 8080}
        ],
        #{env=>#{dispatch=>Dispatch}
    }),

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

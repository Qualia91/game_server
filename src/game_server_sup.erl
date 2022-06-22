%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application supervisor.
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_server_sup).
-author(boc_dev).
-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> supervisor:startlink_ret(). 
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
 
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},

     ChildSpecs = [
        create_child(site_sup, site_sup, [], supervisor),
        create_child(game_telemetry, game_telemetry, [], worker)
    ],
        
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_child(ID, Module, Inputs, Type) ->
    #{
        id => ID,
        start => {Module, start_link, Inputs},
        restart => permanent,
        shutdown => brutal_kill,
        type => Type
    }.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application module.
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_server_app).
-author(boc_dev).
-behaviour(application).

-include("game_server.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
    start/2, 
    stop/1
]).

%%%=============================================================================
%%% API
%%%=============================================================================

start(_StartType, _StartArgs) ->
    ets:new(?IDS_TABLE_NAME, [set, named_table, public]),
    ets:new(?PUBLID_IDS_TABLE_NAME, [set, named_table, public]),
    game_server_sup:start_link().

stop(_State) ->
    ok.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% Websocket handler to allow for collection of telemtry data via websockets.
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(stats_handler).
-author(boc_dev).

-include("game_server.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(loop_state, {
    interval
}).

%%%=============================================================================
%%% API
%%%=============================================================================

init(Req, [{stats_interval, SInterval}]) ->
    {cowboy_websocket, Req, #loop_state{
        interval = SInterval
    }}.

websocket_init(LoopState = #loop_state{interval = SInterval}) ->
    lager:debug("Stats WS Connected"),
    erlang:start_timer(SInterval, self(), ping),
    {reply, {binary, <<"ping">>}, LoopState}.

websocket_handle({_, <<"pong">>}, LoopState = #loop_state{interval = SInterval}) ->
    erlang:start_timer(SInterval, self(), ping),
    {ok, LoopState};
websocket_handle({_, <<"get_stats">>}, LoopState) ->
    Resp = create_response(
        "{\"server_version\": ~p, \"server_start_datetime\": ~p, \"processes_running\": ~p, \"game_running\": ~p, \"games_played\": ~p, \"server_busy\": ~p, \"public_lobbies_created\": ~p, \"private_lobbies_created\": ~p, \"websocket_connections\": ~p}", 
        [
            game_telemetry:get_server_version(), 
            game_telemetry:get_server_start_datetime(), 
            game_telemetry:get_number_of_processes(), 
            game_telemetry:get_games_running(), 
            game_telemetry:get_games_played(), 
            game_telemetry:get_server_busy(), 
            game_telemetry:get_public_lobbies_created(),
            game_telemetry:get_private_lobbies_created(),
            game_telemetry:get_websocket_connections()
        ]),
    {reply,  {text, Resp}, LoopState};
websocket_handle(Msg, LoopState) ->
    lager:debug("Unhandled Json: ~p~n", [Msg]),
    {ok, LoopState}.

websocket_info({timeout, _Ref, ping}, LoopState) ->
    {reply, {text, <<"ping">>}, LoopState};
websocket_info(Info, State) ->
    lager:debug("Unknown Notification recieved: ~p", [Info]),
    {ok, State}.

terminate(_Reason, _Req, _LoopState) ->
    lager:debug("Ending Stats Connection"),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_response(Str, Inputs) ->
    list_to_binary(io_lib:format(Str, Inputs)).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-endif.
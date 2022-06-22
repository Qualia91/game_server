%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_test_SUITE).
-author(boc_dev).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).
-export([
    lobby_create_and_stop_test/1,
    client_join_test/1,
    client_join_open_test/1,
    create_and_find_games_test/1
]).
 
%%%=============================================================================
%%% Tests
%%%=============================================================================

all() -> 
    [lobby_create_and_stop_test, client_join_test, client_join_open_test, create_and_find_games_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(game_server),
    {ok, _} = application:ensure_all_started(gun),
    Config.

init_per_testcase(_, Config) ->
    Config.
 
end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    application:stop(game_server),
    application:stop(gun),
    Config.
 
lobby_create_and_stop_test(_Config) ->
    {Pid, Ref} = wsConnect(),

    gun:ws_send(Pid, {text, "{\"msg\": \"create_game\", \"type\": \"private\", \"user_name\": \"User Name\"}"}),

    listener({binary, <<"ping">>}),
    {text, Msg} = listener(),

    {struct,[{<<"lobby_created">>, _}]} = mochijson2:decode(Msg),

    ?assertEqual(1, game_telemetry:get_games_running(), <<"1 game running">>),
    
    wsClose(Pid, Ref),

    timer:sleep(500),
    
    ?assertEqual(1, game_telemetry:get_games_played(), <<"1 game has been created">>),
    ?assertEqual(0, game_telemetry:get_server_busy(), <<"0 server busy requests">>),
    ?assertEqual(0, game_telemetry:get_games_running(), <<"0 game running">>).

client_join_test(_Config) ->
    {Pid, Ref} = wsConnect(),

    gun:ws_send(Pid, {text, "{\"msg\": \"create_game\", \"type\": \"private\", \"user_name\": \"User Name\"}"}),

    listener({binary, <<"ping">>}),
    {text, Msg} = listener(),

    {struct,[{<<"lobby_created">>, LobbyName}]} = mochijson2:decode(Msg),

    ?assertEqual(1, game_telemetry:get_games_running(), <<"1 game running">>),

    spawn_monitor(fun() -> client_process(LobbyName, <<"private">>) end),

    listener({text, <<"{\"user_joined\":\"Client Name\"}">>}),
    
    wsClose(Pid, Ref),

    timer:sleep(500),
    
    ?assertEqual(2, game_telemetry:get_games_played(), <<"2 game has been created">>),
    ?assertEqual(0, game_telemetry:get_server_busy(), <<"0 server busy requests">>),
    ?assertEqual(0, game_telemetry:get_games_running(), <<"0 game running">>).

client_join_open_test(_Config) ->
    {Pid, Ref} = wsConnect(),

    gun:ws_send(Pid, {text, "{\"msg\": \"create_game\", \"type\": \"public\", \"user_name\": \"User Name\"}"}),

    listener({binary, <<"ping">>}),
    {text, Msg} = listener(),

    {struct,[{<<"lobby_created">>, LobbyName}]} = mochijson2:decode(Msg),

    spawn_monitor(fun() -> client_process(LobbyName, <<"public">>) end),

    listener({text, <<"{\"user_joined\":\"Client Name\"}">>}),
    
    wsClose(Pid, Ref),

    timer:sleep(500),
    
    ?assertEqual(0, game_telemetry:get_server_busy(), <<"0 server busy requests">>),
    ?assertEqual(0, game_telemetry:get_games_running(), <<"0 game running">>).

create_and_find_games_test(_Config) ->
    {Pid, Ref} = wsConnect(),

    gun:ws_send(Pid, {text, "{\"msg\": \"create_game\", \"type\": \"public\", \"user_name\": \"User Name\"}"}),

    listener({binary, <<"ping">>}),
    {text, Msg} = listener(),

    {struct,[{<<"lobby_created">>, LobbyName}]} = mochijson2:decode(Msg),

    gun:ws_send(Pid, {text, "{\"msg\": \"get_public_games\"}"}),

    Resp = list_to_binary(io_lib:format("{\"games\": [<<\"~s\">>]}", [LobbyName])),
    listener({text, Resp}),
    
    wsClose(Pid, Ref),

    timer:sleep(500),
    
    ?assertEqual(0, game_telemetry:get_server_busy(), <<"0 server busy requests">>),
    ?assertEqual(0, game_telemetry:get_games_running(), <<"0 game running">>).
 
%%%=============================================================================
%%% Internal
%%%=============================================================================

wsConnect() ->
   {ok, Pid} = gun:open("localhost", 8080),
   {ok, http} = gun:await_up(Pid),
   Ref = monitor(process, Pid),
   gun:ws_upgrade(Pid, "/game_handler", [], #{compress => true}),
   receive
      {gun_upgrade, _ConnPid, _StreamRef, _Other, Headers} ->
        ok;
      Msg ->
        ct:print("Unexpected start message ~p", [Msg]),
        error(failed)
    after 1000 ->
        exit(timeout)
   end,
   {Pid, Ref}.

wsClose(Pid, Ref) ->
   demonitor(Ref),
   gun:close(Pid),
   gun:flush(Pid).

listener() ->
   receive
    {gun_ws, _, _, ReceivedFrame} ->
        ReceivedFrame;
    Msg ->
        ct:print("Unexpected listener message ~p", [Msg])
    after 1000 ->
        exit(timeout)
   end.
listener(ExpectedFrame) ->
   receive
    {gun_ws, _, _, ReceivedFrame} ->
        ?assertEqual(ExpectedFrame, ReceivedFrame),
        ReceivedFrame;
    Msg ->
        ct:print("Unexpected listener test message ~p", [Msg])
    after 1000 ->
        exit(timeout)
   end.

client_process(LobbyName, Type) ->
    {Pid, Ref} = wsConnect(),
    Msg = io_lib:format("{\"msg\": \"join_game\", \"type\": \"~s\", \"lobby_name\": \"~s\", \"user_name\": \"Client Name\"}", [Type, LobbyName]),
    gun:ws_send(Pid, {text, Msg}),

    listener({binary, <<"ping">>}),
    {text, RespMsg} = listener(),

    Resp = mochijson2:decode(RespMsg),
    
    wsClose(Pid, Ref).

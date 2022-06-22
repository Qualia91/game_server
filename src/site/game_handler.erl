%%%-----------------------------------------------------------------------------
%%% @doc
%%% Game server handler, managing all client connections to the server.
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_handler).
-author(boc_dev).

-include("game_server.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-define(SM, <<"\"">>).
-define(COMMA, <<",">>).
-define(CLOSE_SQR, <<"]">>).
-define(CLOSE_CRL, <<"}">>).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(loop_state, {
    interval,
    server_name,
    user_index,
    user_name
}).

%%%=============================================================================
%%% API
%%%=============================================================================

init(Req, [{stats_interval, SInterval}]) ->
    {cowboy_websocket, Req, #loop_state{
        interval = SInterval
    }}.

websocket_init(LoopState = #loop_state{interval = SInterval}) ->
    lager:debug("WS Connected"),
    erlang:start_timer(SInterval, self(), ping),
    {reply, {binary, <<"ping">>}, LoopState}.

websocket_handle({_, <<"pong">>}, LoopState = #loop_state{interval = SInterval}) ->
    erlang:start_timer(SInterval, self(), ping),
    {ok, LoopState};
websocket_handle({_, Msg}, LoopState) ->
    Json = mochijson2:decode(Msg),
    handle_json_and_respond(Json, LoopState);
websocket_handle(Msg, LoopState) ->
    lager:debug("Unhandled Json: ~p~n", [Msg]),
    {ok, LoopState}.

websocket_info({timeout, _Ref, ping}, LoopState) ->
    {reply, {text, <<"ping">>}, LoopState};
websocket_info({gproc_ps_event, _LobbyName, Msg}, LoopState) when is_list(Msg) ->
    {reply, {text, mochijson2:encode({struct, Msg})}, LoopState};
websocket_info({gproc_ps_event, _LobbyName, Msg}, LoopState) ->
    {reply, {text, mochijson2:encode({struct, [Msg]})}, LoopState};
websocket_info(Info, State) ->
    lager:debug("Unknown Notification recieved: ~p", [Info]),
    {ok, State}.

terminate(_Reason, _Req, #loop_state{server_name = ServerName}) ->
    Resp = game_server_pub_sub:end_game(ServerName),
    case Resp of
        {error, game_not_found} ->
            ok;
        _ ->
            game_telemetry:game_ended()
    end,
    case game_server_pub_sub:is_game_public(ServerName) of
        true -> 
            game_server_pub_sub:close_to_public(ServerName);
        false ->
            ok
    end,
    lager:debug("Ending Game ~p: ~p", [ServerName, Resp]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_json_and_respond({struct, [{<<"version">>, GameVersion}]}, LoopState) ->
    {ok, ServerVersion} = application:get_env(game_server, server_version),
    case GameVersion of
        ServerVersion ->
            {reply, {text, create_response("{\"version_match\": ~s}",  ["true"])}, LoopState};
        _Other ->
            {reply, {text, create_response("{\"version_match\": ~s}",  ["false"])}, LoopState}
    end;

handle_json_and_respond({struct, [{<<"msg">>, <<"create_game">>}, {<<"type">>, <<"private">>}, {<<"user_name">>, UserName}]}, LoopState) ->
    case game_server_pub_sub:create_new_game() of
        {error, server_busy} ->
            game_telemetry:server_busy(),
            {reply, {text, create_response("{\"error\": \"~s\"}",  ["server_busy"])}, LoopState};
        LobbyName ->
            game_telemetry:game_started(),
            game_telemetry:private_lobby_created(),
            {reply, {text, create_response("{\"lobby_created\": \"~s\"}",  [LobbyName])}, LoopState#loop_state{server_name = LobbyName, user_name = UserName, user_index = 0}}
    end;

handle_json_and_respond({struct, [{<<"msg">>, <<"create_game">>}, {<<"type">>, <<"public">>}, {<<"user_name">>, UserName}]}, LoopState) ->
    case game_server_pub_sub:create_new_game() of
        {error, server_busy} ->
            game_telemetry:server_busy(),
            {reply, {text, create_response("{\"error\": \"~s\"}",  ["server_busy"])}, LoopState};
        LobbyName ->
            game_telemetry:game_started(),
            game_telemetry:public_lobby_created(),
            game_server_pub_sub:open_to_public(LobbyName),
            {reply, {text, create_response("{\"lobby_created\": \"~s\"}",  [LobbyName])}, LoopState#loop_state{server_name = LobbyName, user_name = UserName, user_index = 0}}
    end;

handle_json_and_respond({struct, [{<<"msg">>, <<"join_game">>}, {<<"type">>, <<"private">>}, {<<"lobby_name">>, LobbyName}, {<<"user_name">>, UserName}]}, LoopState) ->
    join_game(LoopState, LobbyName, UserName);

handle_json_and_respond({struct, [{<<"msg">>, <<"join_game">>}, {<<"type">>, <<"public">>}, {<<"lobby_name">>, LobbyName}, {<<"user_name">>, UserName}]}, LoopState) ->
    case game_server_pub_sub:is_game_public(LobbyName) of
        true -> 
            game_server_pub_sub:close_to_public(LobbyName),
            join_game(LoopState, LobbyName, UserName);
        false ->
            {reply, {text, create_response("{\"error\": \"~s\"}",  [<<"Game is not public">>])}, LoopState}
    end;

handle_json_and_respond({struct, [{<<"msg">>, <<"join_game">>}, {<<"type">>, <<"private">>}, {<<"lobby_name">>, LobbyName}, {<<"user_name">>, UserName}]}, LoopState) ->
    join_game(LoopState, LobbyName, UserName);

handle_json_and_respond({struct, [{<<"msg">>, <<"get_public_games">>}]}, LoopState) ->
    {reply, {text, create_response_list(game_server_pub_sub:get_public_games())}, LoopState};

handle_json_and_respond({struct, Msg}, LoopState = #loop_state{server_name = ServerName, user_index = UserIndex}) ->
    game_server_pub_sub:publish(ServerName, [{user_index, UserIndex} | Msg]),
    {ok, LoopState}.

create_response_list([]) ->
    <<"{\"games\":[]}">>;
create_response_list(List) ->
    lists:foldl(
    fun(BinStr, {Acc, ElemsLeft}) ->
        case ElemsLeft of
            1 ->
                <<Acc/binary, ?SM/binary, BinStr/binary, ?SM/binary, ?CLOSE_SQR/binary, ?CLOSE_CRL/binary>>;
            _ ->
                {<<Acc/binary, ?SM/binary, BinStr/binary, ?SM/binary, ?COMMA/binary>>, ElemsLeft - 1}
        end
    end,
    {<<"{\"games\":[">>, length(List)},
    List).

create_response(Str, Inputs) ->
    list_to_binary(io_lib:format(Str, Inputs)).

join_game(LoopState, LobbyName, UserName) ->
    case game_server_pub_sub:join_game(LobbyName, UserName) of
        {error, Reason} ->
            lager:info("Can't join lobby with name ~p because: ~p", [LobbyName, Reason]),
            {reply, {text, create_response("{\"error\": \"~s\"}",  [Reason])}, LoopState};
        _ ->
            {reply, {text, create_response("{\"user_joined\": \"~s\"}",  [UserName])}, LoopState#loop_state{server_name = LobbyName, user_name = UserName, user_index = 1}}
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

create_response_test() ->
    ?assertMatch(<<"Test message 123">>, create_response("Test message ~p", [123]), <<"Converting string and number">>).

handle_json_and_respond_create_game_test() ->
    TestLoopState = #loop_state{},

    Mecks = [game_server_pub_sub, game_telemetry],

    meck:new(Mecks, []),

    meck:expect(game_telemetry, server_busy, fun() -> ok end),
    meck:expect(game_telemetry, get_games_running, fun() -> ok end),
    meck:expect(game_telemetry, get_server_busy, fun() -> ok end),
    meck:expect(game_telemetry, game_ended, fun() -> ok end),
    meck:expect(game_telemetry, game_started, fun() -> ok end),
    meck:expect(game_telemetry, private_lobby_created, fun() -> ok end),
    meck:expect(game_telemetry, public_lobby_created, fun() -> ok end),
    meck:expect(game_server_pub_sub, create_new_game, fun() -> <<"ABCDE">> end),

    {reply, {text, ResponsePass}, _} = handle_json_and_respond({struct, [{<<"msg">>, <<"create_game">>}, {<<"type">>, <<"private">>}, {<<"user_name">>, <<"USER_NAME">>}]}, TestLoopState),

    ?assertMatch(<<"{\"lobby_created\": \"ABCDE\"}">>, ResponsePass, <<"Creating game successfully return">>),

    meck:expect(game_server_pub_sub, create_new_game, fun() -> {error, server_busy} end),

    {reply, {text, ResponseFail}, _} = handle_json_and_respond({struct, [{<<"msg">>, <<"create_game">>}, {<<"type">>, <<"private">>}, {<<"user_name">>, <<"USER_NAME">>}]}, TestLoopState),

    ?assertMatch(<<"{\"error\": \"server_busy\"}">>, ResponseFail, <<"Creating game unsuccessfully return">>),
    
    meck:unload(Mecks).

handle_json_and_respond_create_public_game_test() ->
    TestLoopState = #loop_state{},

    Mecks = [game_server_pub_sub, game_telemetry],

    meck:new(Mecks, []),

    meck:expect(game_telemetry, server_busy, fun() -> ok end),
    meck:expect(game_telemetry, get_games_running, fun() -> ok end),
    meck:expect(game_telemetry, get_server_busy, fun() -> ok end),
    meck:expect(game_telemetry, game_ended, fun() -> ok end),
    meck:expect(game_telemetry, game_started, fun() -> ok end),
    meck:expect(game_telemetry, private_lobby_created, fun() -> ok end),
    meck:expect(game_telemetry, public_lobby_created, fun() -> ok end),
    meck:expect(game_server_pub_sub, create_new_game, fun() -> <<"ABCDE">> end),
    meck:expect(game_server_pub_sub, open_to_public, fun(_) -> ok end),
    meck:expect(game_server_pub_sub, close_to_public, fun(_) -> ok end),
    meck:expect(game_server_pub_sub, is_server_name_taken, fun(LobbyName) -> LobbyName end),

    {reply, {text, ResponsePass}, _} = handle_json_and_respond({struct, [{<<"msg">>, <<"create_game">>}, {<<"type">>, <<"public">>}, {<<"user_name">>, <<"USER_NAME">>}]}, TestLoopState),

    ?assertMatch(<<"{\"lobby_created\": \"ABCDE\"}">>, ResponsePass, <<"Creating open game successfully return">>),
    
    meck:unload(Mecks).

create_response_list_test() ->
    ?assertMatch(<<"{\"games\":[\"a\",\"b\",\"c\"]}">>, create_response_list([<<"a">>, <<"b">>, <<"c">>]), <<"Full list conversion">>),
    ?assertMatch(<<"{\"games\":[]}">>, create_response_list([]), <<"Full list conversion">>).

-endif.
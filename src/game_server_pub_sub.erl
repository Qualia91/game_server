%%%-----------------------------------------------------------------------------
%%% @doc
%%% Module containing all the functionality of the game server behind the scenes.
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(game_server_pub_sub).
-author(boc_dev).

-include("game_server.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-define(SERVER, ?MODULE).

-export([
    create_new_game/0,
    join_game/2,
    leave_game/2,
    end_game/1,
    publish/2,
    is_server_name_taken/1,
    open_to_public/1,
    close_to_public/1,
    get_public_games/0,
    is_game_public/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Creates a new game with a unique game. if server is busy and a unique name 
%% cannot be created, it returns {error, server_busy}. Used by game host.
%% @end
%%------------------------------------------------------------------------------
-spec create_new_game() -> binary() | {error, server_busy}. 
create_new_game() ->
    case repeate(3, fun create_unique_name/0) of
        {error, retries_exceeded} -> 
            {error, server_busy};
        UniqueName ->
            create_game_name(UniqueName),
            gproc_ps:subscribe(l, UniqueName),
            UniqueName
    end.
    
%%------------------------------------------------------------------------------
%% @doc
%% Attempts to join a game with ID ServerName, using the user name UserName. 
%% If not found, returns {error, game_not_found}. Otherwise, returns ok and 
%% subscribes user process to game server publisher.
%% @end
%%------------------------------------------------------------------------------
-spec join_game(binary(), binary()) -> ok | {error, game_not_found}. 
join_game(ServerName, UserName) ->
    case is_server_name_taken(ServerName) of
        {error, already_taken} ->
            gproc_ps:subscribe(l, ServerName),
            publish(ServerName, {user_joined, UserName});
        _ ->
            {error, game_not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Attempts to leave a game with ID ServerName, using the user name UserName. 
%% If not found, returns {error, game_not_found}. Otherwise, returns true and 
%% unsubscribes user process to game server publisher.
%% @end
%%------------------------------------------------------------------------------
-spec leave_game(binary(), binary()) -> true | {error, game_not_found}. 
leave_game(ServerName, UserName) ->
    case is_server_name_taken(ServerName) of
        {error, already_taken} -> 
            publish(ServerName, {user_left, UserName}),
            gproc_ps:unsubscribe(l, ServerName);
        _ ->
            {error, game_not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Attempts to end the game. If not found, returns {error, game_not_found}.
%% Otherwise, unsubscribes user process from publisher, publishers a game ended 
%% message to on game publish space, and deletes the game name from the ids table.
%% @end
%%------------------------------------------------------------------------------
-spec end_game(binary()) -> true | {error, game_not_found}. 
end_game(ServerName) ->
    case is_server_name_taken(ServerName) of
        {error, already_taken} -> 
            gproc_ps:unsubscribe(l, ServerName),
            publish(ServerName, {game_ended, ServerName}),
            delete_game_name(ServerName);
        _ ->
            {error, game_not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Publishes a message on the server space with name ServerName.
%% @end
%%------------------------------------------------------------------------------
-spec publish(binary(), any()) -> ok. 
publish(ServerName, Msg) ->
    gproc_ps:publish(l, ServerName, Msg).

%%------------------------------------------------------------------------------
%% @doc
%% Checks if a server name is already taken. Retunrs server name if not.
%% @end
%%------------------------------------------------------------------------------
-spec is_server_name_taken(binary()) -> binary() | {error, game_not_found}. 
is_server_name_taken(LobbyName) ->
    case ets:member(?IDS_TABLE_NAME, LobbyName) of
        true -> {error, already_taken};
        false -> LobbyName
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Turns a private game into a public game by adding it onto the public lobbies 
%% ets table.
%% @end
%%------------------------------------------------------------------------------
-spec open_to_public(binary()) -> boolean(). 
open_to_public(LobbyName) ->
    ets:insert_new(?PUBLID_IDS_TABLE_NAME, {LobbyName, true}).

%%------------------------------------------------------------------------------
%% @doc
%% Turns a public game into a private game by removing it onto the public lobbies 
%% ets table.
%% @end
%%------------------------------------------------------------------------------
-spec close_to_public(binary()) -> true. 
close_to_public(LobbyName) ->
    ets:delete(?PUBLID_IDS_TABLE_NAME, LobbyName).

%%------------------------------------------------------------------------------
%% @doc
%% Gets a list of public games available.
%% @end
%%------------------------------------------------------------------------------
-spec get_public_games() -> list(binary()). 
get_public_games() ->
    lists:foldl(
        fun({Val, _}, Acc) ->
            [Val | Acc]
        end,
        [],
        ets:tab2list(?PUBLID_IDS_TABLE_NAME)).

%%------------------------------------------------------------------------------
%% @doc
%% Checks if a game is public or not.
%% @end
%%------------------------------------------------------------------------------
-spec is_game_public(binary()) -> boolean(). 
is_game_public(LobbyName) ->
    ets:member(?PUBLID_IDS_TABLE_NAME, LobbyName).

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_game_name(LobbyName) ->
    ets:insert_new(?IDS_TABLE_NAME, {LobbyName, true}).
    
delete_game_name(LobbyName) ->
    ets:delete(?IDS_TABLE_NAME, LobbyName).

create_unique_name() ->
    is_server_name_taken(get_random_game_name()).

get_random_game_name() ->
    get_random_string(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").

get_random_string(Length, AllowedChars) ->
    list_to_binary(lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length))).

repeate(0, _) ->
    {error, retries_exceeded};
repeate(Retries, Function) ->
    case Function() of
        {error, already_taken} -> repeate(Retries - 1, Function);
        Name -> Name
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

repeate_test() ->
    ?assertMatch({error, retries_exceeded}, repeate(0, undefined), <<"0 retries">>),
    ?assertMatch({error, retries_exceeded}, repeate(3, fun() -> {error, already_taken} end), <<"Returns error 3 times">>),
    ?assertMatch(<<"Hello">>, repeate(3, fun() -> <<"Hello">> end), <<"Returns name">>).

get_random_string_test() ->
    ?assertMatch(<<"AAAAA">>, get_random_string(5, "A"), <<"5 A's">>),
    ?assertMatch(5, length(erlang:binary_to_list(get_random_string(5, "ABC"))), <<"Random string of length 5">>).

get_random_game_name_test() ->
    ?assertMatch(5, length(erlang:binary_to_list(get_random_game_name())), <<"Random string of length 5">>).

server_name_storage_test() ->
    ets:new(?IDS_TABLE_NAME, [set, named_table, public]),
    ?assertMatch("newgame", is_server_name_taken("newgame"), <<"Checking name in empty ets table">>),
    LobbyName = create_unique_name(),
    create_game_name(LobbyName),
    ?assertMatch({error, already_taken}, is_server_name_taken(LobbyName), <<"Checking name already taken">>),
    delete_game_name(LobbyName),
    ?assertMatch(LobbyName, is_server_name_taken(LobbyName), <<"Checking name after name was deleted">>),
    ets:delete(?IDS_TABLE_NAME).

api_test() ->
    ets:new(?IDS_TABLE_NAME, [set, named_table, public]),
    ok = application:start(gproc),
    LobbyName = create_new_game(),
    UserName = <<"HELLO">>,
    ?assertNotMatch({error, server_busy}, LobbyName, <<"Checking new game was created">>),
    {_Pid, MonitorRef} = spawn_monitor(fun() -> joining_player_func(LobbyName, UserName) end),
    lobby_leader_func(LobbyName, UserName),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> ok
    end,
    application:stop(gproc),
    ets:delete(?IDS_TABLE_NAME).

public_games_test() ->
    ets:new(?PUBLID_IDS_TABLE_NAME, [set, named_table, public]),
    LobbyName = <<"ABCDE">>,
    open_to_public(LobbyName),
    ?assertMatch(true, is_game_public(LobbyName), <<"game is public after opening">>),
    ?assertMatch([LobbyName], get_public_games(), <<"1 public game">>),
    close_to_public(LobbyName),
    ?assertMatch(false, is_game_public(LobbyName), <<"game is closed after closing">>),
    ?assertMatch([], get_public_games(), <<"0 public game">>),
    ets:delete(?PUBLID_IDS_TABLE_NAME).

%%%===================================================================
%%% Test internal functions
%%%===================================================================

lobby_leader_func(InputLobbyName, InputUserName) ->
    receive
        {gproc_ps_event,LobbyName,{user_joined,UserName}} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in lobby leader message for joining">>),
            ?assertMatch(InputUserName, UserName, <<"Checking user name in lobby leader message for joining">>),
            lobby_leader_func(InputLobbyName, InputUserName);
        {gproc_ps_event,LobbyName,{user_left,UserName}} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in lobby leader for leaving">>),
            ?assertMatch(InputUserName, UserName, <<"Checking user name in lobby leader for leaving">>),
            lobby_leader_func(InputLobbyName, InputUserName);
        {gproc_ps_event,LobbyName,<<"Test Message">>} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in lobby leader recieving message">>),
            lobby_leader_func(InputLobbyName, InputUserName);
        {gproc_ps_event,LobbyName,<<"End of game message">>} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in lobby leader recieving message to quit">>),
            ?assertNotMatch({error, _}, end_game(LobbyName), <<"Checking game can be stopped">>);
        Var ->
            ok
    end.

joining_player_func(LobbyName, UserName) ->
    ?assertNotMatch({error, _}, join_game(LobbyName, UserName), <<"Checking game can be joined">>),
    ?assertNotMatch({error, _}, publish(LobbyName, <<"Test Message">>), <<"Checking client can send message">>),
    ?assertNotMatch({error, _}, leave_game(LobbyName, UserName), <<"Checking game can be left">>),
    ?assertNotMatch({error, _}, join_game(LobbyName, UserName), <<"Checking game can be joined">>),
    ?assertNotMatch({error, _}, publish(LobbyName, <<"End of game message">>), <<"Checking client can send message">>),
    joining_player_func_inner(LobbyName, UserName).

joining_player_func_inner(InputLobbyName, InputUserName) ->
    receive
        {gproc_ps_event,LobbyName,{user_joined,UserName}} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in client message for joining">>),
            ?assertMatch(InputUserName, UserName, <<"Checking user name in client message for joining">>),
            joining_player_func_inner(InputLobbyName, InputUserName);
        {gproc_ps_event,LobbyName,{user_left,UserName}} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in client message for leaving">>),
            ?assertMatch(InputUserName, UserName, <<"Checking user name in client message for leaving">>),
            joining_player_func_inner(InputLobbyName, InputUserName);
        {gproc_ps_event,LobbyName,{game_ended, LobbyName}} ->
            ok;
        {gproc_ps_event,LobbyName,Msg} ->
            ?assertMatch(InputLobbyName, LobbyName, <<"Checking lobby name in client message recieving message">>),
            joining_player_func_inner(InputLobbyName, InputUserName);
        Var ->
            ok
    end.

-endif.
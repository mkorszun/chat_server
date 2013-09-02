%%% @doc Chat room API

-module(chat_server).

-export([start/1, stop/0, join/1, get_people/0, to/2]).

%% ===================================================================
%% Api
%% ===================================================================

%% @doc Stop the chat service on all visible nodes.
stop() ->
    gen_server:cast(chat_room, stop_all).

%% @doc Make the calling process join the chat under the `Nickname'.
join(Nickname) ->
    gen_server:call(chat_room, {join, self(), Nickname}).

%% @doc Get a list of all the nicknames of people connected to the service
get_people() ->
    gen_server:call(chat_room, get_people_all).

%% @doc Send a chat `Message' to the process under nickname `To'.
to(To, Message) ->
    gen_server:call(chat_room, {send, self(), To, Message}).

%% @doc Start the chat service on each node from `Nodes'.
start(Nodes) ->
    lists:foreach(
        fun(Node) ->
            pong = net_adm:ping(Node),
            case rpc:call(Node, chat_server_sup, start_child, [Nodes]) of
                {ok, _Pid} ->
                    io:format("Service successfuly started on ~p~n", [Node]),
                    ok;
                {badrpc, Reason} ->
                    io:format("Failed to start on ~p: ~p~n", [Node, Reason]),
                    throw(failed_to_start_service)
            end
        end,
        Nodes).

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

db_execute_test_() ->
    {setup,
     fun() ->
         meck:new(net_adm, [unstick]),
         meck:expect(net_adm, ping, fun(_) -> pong end),
         ok = application:start(chat_server),
         ok = chat_server:start([node()])
     end,
     fun(_) ->
         true = meck:validate(net_adm),
         meck:unload(net_adm),
         ok = application:stop(chat_server),
         ok = chat_server:stop()
     end,
     [
         fun test/0
     ]
    }.

test() ->
    ?assertEqual([], chat_server:get_people()),
    ?assertEqual({error,sender_not_registered}, chat_server:to("Ghost", "hello")),
    ?assertEqual(ok, chat_server:join("Mateusz")),
    ?assertEqual(["Mateusz"], chat_server:get_people()),
    ?assertEqual({error, already_registered}, chat_server:join("Mateusz2")),
    ?assertEqual({error, receiver_not_found}, chat_server:to("Ghost", "hello")),
    ?assertEqual(ok, chat_server:to("Mateusz", "i am talking to myself")),
    ?assertEqual(ok, receive {chat, "Mateusz", "i am talking to myself"} -> ok; M -> {error, M} end).

-endif.

%% ===================================================================
%% ===================================================================
%% ===================================================================
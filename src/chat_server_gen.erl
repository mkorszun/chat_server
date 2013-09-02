%%% @doc Chat room process implementation

-module(chat_server_gen).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================================================================
%% ===================================================================
%% ===================================================================

-define(NODES(N), lists:filter(fun(Node) -> if node() == Node -> false; true -> true end end, N)).
-record(state, {users = [], nodes = []}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Nodes) ->
    gen_server:start_link({local, chat_room}, ?MODULE, [Nodes], []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([Nodes]) ->
    % Exclude self node from nodes list
    process_flag(trap_exit, true),
    {ok, #state{nodes = ?NODES(Nodes)}}.

%% Handle global get_people request
handle_call(get_people_all, From, #state{nodes = Nodes, users = Users} = State) ->
    spawn_link(fun() ->
        {Replies, []} = gen_server:multi_call(Nodes, chat_room, get_people),
        gen_server:reply(From, lists:merge([[U || {_, U} <- Users] | [P || {_, P} <- Replies]]))
    end),
    {noreply, State};

%% Handle local get_people request
handle_call(get_people, _From, #state{users = Users} = State) ->
    {reply, [U || {_, U} <- Users], State};

%% Handle new users
handle_call({join, Pid, Nickname}, _From, #state{users = Users, nodes = Nodes} = State) ->
    case proplists:is_defined(Pid, Users) of
        false ->
            % Monitor user process
            erlang:monitor(process, Pid),
            io:format("~p is joining the chat room~n", [Nickname]),
            notify_nodes(Nickname, Nodes),
            {reply, ok, State#state{users = [{Pid, Nickname}, Users]}};
        true ->
            {reply, {error, already_registered}, State}
    end;

%% Handle sent messages
handle_call({send, SenderPid, To, Msg}, From, #state{nodes = Nodes, users = Users} = State) ->
    SenderName = proplists:get_value(SenderPid, Users),
    case find_receiver(To, Users, Nodes) of
        {ok, {pid, Pid}} ->
            spawn_link(fun() ->
                gen_server:reply(From, send(local, SenderName, To, Msg, Pid))
            end);
        {ok, {node, Node}} ->
            spawn_link(fun() ->
                gen_server:reply(From, send(remote, SenderName, To, Msg, Node))
            end)
    end,
    {noreply, State};

%% Handle received messages
handle_call({msg, Sender, To, Msg}, From, #state{users = Users} = State) ->
    spawn_link(fun() ->
        gen_server:reply(From, send(local, Sender, To, Msg, get_receiver_pid(To, Users)))
    end),
    {noreply, State};

%% Handle user existence checks
handle_call({exists, Nickname}, _From, #state{users = Users} = State) ->
    {reply, exists(Nickname, Users), State};

%% Handle local termination
handle_call(stop, _From, State) ->
    {stop, normal, State}.

%% Handle join notification
handle_cast({joined, Nickname}, State) ->
    io:format("~p is joining the chat room~n", [Nickname]),
    {noreply, State};

%% Handle global termination
handle_cast(stop_all, #state{nodes = Nodes} = State) ->
    gen_server:multi_call(Nodes, chat_room, stop, 5000),
    {stop, normal, State}.

%% Handle user termination
handle_info({'DOWN', _, process, Pid, _}, #state{users = Users} = State) ->
    io:format("User ~p diconnected~n", [proplists:get_value(Pid, Users)]),
    {noreply, State#state{users = proplists:delete(Pid, Users)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

send(_, undefined, _, _, _) ->
    {error, sender_not_registered};
send(_, _, _, _, undefined) ->
    {error, receiver_not_found};
send(remote, Sender, To, Msg, Node) ->
    io:format("Server on ~p is passing a message from ~p to ~p(~p)~n", [node(), Sender, To, Node]),
    gen_server:call({chat_room, Node}, {msg, Sender, To, Msg});
send(local, Sender, To, Msg, Pid) ->
    io:format("Server on ~p is delivering a message from ~p to ~p(~p)~n", [node(), Sender, To, Pid]),
    Pid ! {chat, Sender, Msg},
    ok.

% Get pid or node
find_receiver(Nickname, Users, Nodes) ->
    case get_receiver_pid(Nickname, Users) of
        undefined ->
            {ok, {node, get_receiver_node(Nickname, Nodes)}};
        Pid ->
            {ok, {pid, Pid}}
    end.

get_receiver_node(Nickname, Nodes) ->
    {Replies, _} = gen_server:multi_call(Nodes, chat_room, {exists, Nickname}),
    case lists:keyfind(true, 2, Replies) of
        {Node, true} ->
            Node;
        false ->
            undefined
    end.

get_receiver_pid(Nickname, Users) ->
    case lists:keyfind(Nickname, 2, Users) of
        {Pid, Nickname} ->
            Pid;
        false ->
            undefined
    end.

exists(Nickname, Users) ->
    case get_receiver_pid(Nickname, Users) of
        undefined ->
            false;
        _ ->
            true
    end.

% Notify nodes about new user
notify_nodes(Nickname, Nodes) ->
    lists:foreach(
        fun(Node) ->
            gen_server:cast({chat_room, Node}, {joined, Nickname})
        end,
    Nodes).

%% ===================================================================
%% ===================================================================
%% ===================================================================
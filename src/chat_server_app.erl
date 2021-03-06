%%% @doc Chat room application

-module(chat_server_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Api
%% ===================================================================

%% @doc Start the chat application.
start() ->
    application:start(chat_server).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    chat_server_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% ===================================================================
%% ===================================================================
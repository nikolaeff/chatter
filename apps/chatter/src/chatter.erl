-module(chatter).
-export([start/0, stop/0]).

start() ->
  chatter_app:start(normal, []).

stop() ->
  chatter_app:stop(normal).
  
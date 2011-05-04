
-module(chatter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
% -define(CHILD(I, Type, Opts), {I, {I, start_link, Opts}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RoomServer = ?CHILD(room_server, worker),
    NickServer = ?CHILD(nick_server, worker),
    WebServer = ?CHILD(web_server, worker),
    {ok, { {one_for_one, 5, 10}, [RoomServer, NickServer, WebServer]} }.


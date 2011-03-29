-module(room_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {name="MAIN", users=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, join/1, leave/1, message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(User) ->
  gen_server:call(?SERVER, {join, User}).

leave(User) ->
  gen_server:call(?SERVER, {leave, User}).

message(FromUser, Message) ->
  gen_server:call(?SERVER, {message, FromUser, Message}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #state{}}.

handle_call({join, User}, _From, State) ->
  {reply, ok, State#state{users=[User | State#state.users]}};

handle_call({leave, User}, _From, State) ->
  {reply, ok, State#state{users=lists:delete(User, State#state.users)}};

handle_call({message, FromUser, Message}, _From, State) ->
  lists:map(fun(User) -> send_message(User, FromUser, Message) end, State#state.users),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_message(User, FromUser, Message) ->
  io:format("User:~p, From: ~p, Msg: ~p", [User, FromUser, Message]),
  ok.

  
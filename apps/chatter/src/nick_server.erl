-module(nick_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {registry, pid_to_login}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, register/2, login/2, check/1, register_pid/2, lookup_pid/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Login, Password) ->
  gen_server:call(?SERVER, {register, Login, Password}).

login(Login, Password) ->
  gen_server:call(?SERVER, {login, Login, Password}).

check(Login) ->
  gen_server:call(?SERVER, {check, Login}).

register_pid(Pid, Login) ->
  gen_server:call(?SERVER, {register_pid, Pid, Login}).

lookup_pid(Pid) ->
  gen_server:call(?SERVER, {lookup_pid, Pid}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Registry = ets:new(nick_registry, [set]),
  PidToLogin = ets:new(pid_to_login, [set]),
  {ok, #state{registry=Registry, pid_to_login=PidToLogin}}.


handle_call({register, Login, Password}, _From, State) ->
  case ets:lookup(State#state.registry, Login) of
    [] -> 
      ets:insert(State#state.registry, {Login, Password}),
      {reply, ok, State};
    _ ->
      {reply, exist, State}
    end;

handle_call({login, Login, Password}, _From, State) ->
  case ets:lookup(State#state.registry, Login) of
    [{Login, Password}] -> {reply, ok, State};
    [{Login, _}] -> {reply, badpassword, State};
    [] -> {reply, notfound, State}
    end;

handle_call({check, Login}, _From, State) ->
  case ets:lookup(State#state.registry, Login) of
    [] -> {reply, notexist, State};
    _  -> {reply, exist, State}
  end;

handle_call({register_pid, Pid, Login}, _From, State) ->
  ets:insert(State#state.pid_to_login, {Pid, Login}),
  {reply, ok, State};

handle_call({lookup_pid, Pid}, _From, State) ->
  case ets:lookup(State#state.pid_to_login, Pid) of
    [] -> {reply, "%username%", State};
    [{Pid, Login}] -> {reply, Login, State}
  end;

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


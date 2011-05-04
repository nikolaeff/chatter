-module(web_server).

-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/0, stop/0]).

% records
-record(state, {
  port
}).

% macros
-define(SERVER, ?MODULE).


% Function: {ok,Pid} | ignore | {error, Error}
start_link() ->
  Port = 3000,
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
  gen_server:cast(?SERVER, stop).

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([Port]) ->
  process_flag(trap_exit, true),
  misultin:start_link([
    {port, Port},
    {loop, fun(Req) -> handle_http(Req, Port) end},
    {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
    {ws_autoexit, false}]),
  erlang:monitor(process, misultin),
  io:format("Web server started."),
  {ok, #state{port = Port}}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%                    {noreply, State} | {noreply, State, Timeout} |
%                    {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% handle info when misultin server goes down -> take down misultin_gen_server too [the supervisor will take everything up again]
handle_info({'DOWN', _, _, {misultin, _}, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
  misultin:stop(),
  terminated.

% ----------------------------------------------------------------------------------------------------------
% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_http(Req, Port) ->
  {ok, Content} = index_dtl:render([{port, Port}, {server, "127.0.0.1"}, {req, Req}]),
  Req:ok([{"Content-Type", "text/html"}], [Content]).


-define(PING_MESSAGE, "{\"type\":\"ping\"}").
-define(PONG_MESSAGE, "{\"type\":\"pong\"}").
-define(OK_MESSAGE,   "{\"type\":\"ok\"}").

%% ejson:encode({
%%  [{type, reply}, {data, ok}]
%% })

handle_websocket(Ws) ->
  receive
    {browser, Data} ->
      io:format("WS RECEIVED: ~p~n", [Data]),
      % TODO: Can throw exception on bad json
      {JSON} = ejson:decode(Data),
      case proplists:get_value(<<"type">>, JSON) of

        <<"message">> ->
          Message = proplists:get_value(<<"data">>, JSON),
          io:format("message received: ~p~n", [Message]),
          Username = nick_server:lookup_pid(self()),
          room_server:message(Username, Message),
          Ws:send([?OK_MESSAGE]);

        <<"login">> ->
          {Payload} = proplists:get_value(<<"data">>, JSON),
          UserName = binary_to_list(proplists:get_value(<<"username">>, Payload)),
          Password = binary_to_list(proplists:get_value(<<"password">>, Payload)),
          io:format("Login attempt username: '~p' pass: '~p'~n", [UserName, Password]),
          case nick_server:login(UserName, Password) of
            badpassword ->
              Ws:send(ejson:encode({ [{type, loggedin}, {data, <<"failure">>}]}));
            ok ->
              nick_server:register_pid(self(), UserName),
              Ws:send(ejson:encode({ [{type, loggedin}, {data, <<"success">>}]}));
            notfound ->
              nick_server:register(UserName, Password),
              nick_server:register_pid(self(), UserName),
              Ws:send(ejson:encode({ [{type, loggedin}, {data, <<"success">>}]}))
          end;
        
        %% join or create room
        <<"join">> ->
          Room = proplists:get_value(<<"room">>, JSON),
          io:format("user want to join room: ~p~n", [Room]),
          % TODO: Sorry mr User, we have only one room now.
          room_server:join(self()),
          Ws:send([?OK_MESSAGE]);
        
        <<"leave">> ->
          Room = proplists:get_value(<<"room">>, JSON),
          io:format("user want to leave room: ~p~n", [Room]),
          room_server:leave(self()),
          Ws:send([?OK_MESSAGE]);
        
        <<"listrooms">> ->
          io:format("user want to list rooms~n"),
          Ws:send([?OK_MESSAGE]);
        
        <<"ping">> ->
          io:format("ping received~n"),
          Ws:send([?PONG_MESSAGE]);
        
        <<"pong">> ->
          io:format("pong received~n");
        
        undefined ->
          io:format("unknown json message received ~p~n", [JSON])
      
      end,
      handle_websocket(Ws);
    
    {message, Message} ->
      io:format("Sending message to user: ~p~n", [Message]),
      BinMessage = list_to_binary(Message),
      Ws:send(ejson:encode({ [{type, message}, {data, BinMessage}] })),
      handle_websocket(Ws);
  
    closed ->
      io:format("User left chat~p~n", [self()]),
      room_server:leave(self()),
      closed;
    
    _Ignore ->
      io:format("WS UNKNOWN MSG ~p~n", [_Ignore]),
      handle_websocket(Ws)
    
    %% 30 sec timeout
    after 30000 ->
      Ws:send([?PING_MESSAGE]),
      handle_websocket(Ws)
    end.
  % recv
-module(web_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

start_link() ->
  Port = 8080,
	misultin:start_link([
	  {port, Port},
	  {loop, fun(Req) -> handle_http(Req, Port) end}, 
	  {ws_loop, fun(Ws) -> handle_websocket(Ws) end}]).

handle_http(Req, Port) ->
	Req:ok([{"Content-Type", "text/html"}],
	["	
	<html>
		<head>
			<script type=\"text/javascript\">
				function addStatus(text){
					var date = new Date();
					document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + date + \": \" + text + \"<br>\";				
				}
				function ready(){
					if (\"WebSocket\" in window) {
						// browser supports websockets
						var ws = new WebSocket(\"ws://localhost:", integer_to_list(Port) ,"/service\");
						ws.onopen = function() {
							// websocket is connected
							addStatus(\"websocket connected!\");
							// send hello data to server.
							ws.send(\"hello server!\");
							addStatus(\"sent message to server: 'hello server'!\");
						};
						ws.onmessage = function (evt) {
							var receivedMsg = evt.data;
							addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
						};
						ws.onclose = function() {
							// websocket was closed
							addStatus(\"websocket was closed\");
						};
					} else {
						// browser does not support websockets
						addStatus(\"sorry, your browser does not support websockets.\");
					}
				}
			</script>
		</head>
		<body onload=\"ready();\">
			<div id=\"status\"></div>
		</body>
	</html>"]).

% callback on received websockets data
handle_websocket(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(["received '", Data, "'"]),
			handle_websocket(Ws);
		_Ignore ->
			handle_websocket(Ws)
	after 5000 ->
		Ws:send("pushing!"),
		handle_websocket(Ws)
	end.

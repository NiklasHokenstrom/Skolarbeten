%% SOURCE:
%% http://erlycoder.com/25/erlang-tcp-client-server-sockets-with-gentcp
%%

%% ERLANG ECHO-TCP-SERVER:
%% 1> c(tcp_echo).
%% {ok,tcp_echo}
%% 2> tcp_echo:listen(4020).
%% <0.38.0>
%%
%% 
%% JAVA TCP CLIENT:
%% > javac ClientSocket.java
%% > java ClientSocket
%%
%% Connecting to server on port 4020
%% Just connected to /127.0.0.1:4020
%% Client received: Hello from /127.0.0.1:54215 from Server
%% 

-module(tcp_echo).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PORT, 4020).

% Call echo:listen() to start the server.
listen(Port) ->
    mnesia:start(),
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> accept(LSocket) end).

% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
			io:format("Connection accepted ~n", []),
			loop(Socket)
		end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    io:format("Got packet: ~p~n", [Data]),

	    Term = binary_to_term(Data),
	    Reply = do_call(Term),
	    %% Send 4 bytes (32 bits) first to declare the size
	    %% Java doesn't handle unsigned ints, so 8 bits
	    %% range between -127 and 127
	  %%  Bin = <<4:32,1:8, 128:8, 123:8, 45:8>>,
	    io:format("Reply: ~p", [Reply]),
	    %%Answer = term_to_binary(Reply),
	    %%Size = byte_size(Answer),
	    %%gen_tcp:send(Socket, integer_to_binary(Size)),
	    gen_tcp:send(Socket, term_to_binary(Reply)),
	    loop(Socket);
	{tcp_closed, Socket}->
	    io:format("Socket ~p closed~n", [Socket]);
	{tcp_error, Socket, Reason} ->
	    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({add, Server_Name, Ip})  ->  bank:add(Server_Name, Ip);
the_func({remove, Server_Name}) ->  bank:remove(Server_Name);
the_func({available}) ->  bank:available();
the_func({clear}) ->  bank:clear();
the_func({ping, Server_Name}) -> bank:ping(Server_Name);
the_func({addPlayer, PlayerName}) -> game_logic:addPlayer(PlayerName);
the_func({removePlayer, PlayerName}) -> game_logic:removePlayer(PlayerName);
the_func({getPos, PlayerName}) -> game_logic:getPos(PlayerName);
the_func({getAllPos}) -> game_logic:getAllPos();
the_func({move, PlayerName, Direction, Amount}) -> game_logic:move(PlayerName, Direction, Amount).

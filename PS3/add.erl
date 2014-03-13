%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, print/4]).

%% @doc start adds the numbers represented in A and B. A and B are in base Base
-spec start(A,B,Base) -> ok when 
      A::string(),
      B::string(), 
      Base::string().

start(A,B, Base) ->
    start(A,B,Base, []).
%% @doc Adds the possibility for options such as selective addition, random sleep and number of list segments
-spec start(A,B,Base, Options) -> ok when 
      A::string(),
      B::string(), 
      Base::string(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    Selective = length([S || S <- Options, S =:= selective]) =:= 1,
    Split = hd([S || {T,S} <- Options, T =:= split] ++ [1]),
    {MinSleep, MaxSleep} = hd([{Min, Max} || {Min, Max} <- Options, Min =/= split] ++ [{0,0}]),

    Al = utils:expandList(utils:convertToTen(Base, A), []),
    Bl = utils:expandList(utils:convertToTen(Base, B), []),
    {Aa, Bb} = utils:addZeroToShortest(Al,Bl),
    {C,R} = listAdd(utils:split(Aa,Split), utils:split(Bb,Split), MinSleep, MaxSleep, Selective),
    case hd(C) =:= 1 of
	true ->
	    print(C, Al, Bl, [1] ++ R);
	false ->
	    print(C, Al, Bl, R)
    end.

%% @doc listAdd adds the numbers in A and B. This can be done selectively. After each addition the process will sleep for a random time between MinSleep and MaxSleep
-spec listAdd(A,B,MinSleep,MaxSleep,Selective) -> {C,R} when
      A::[integer()],
      B::[integer()],
      MinSleep::integer(),
      MaxSleep::integer(),
      Selective::boolean(),
      C::[integer()],
      R::[integer()].

listAdd(A,B, MinSleep, MaxSleep, Selective) ->
    {R,C} = listAdd(A, B, length(A), MinSleep, MaxSleep, Selective),
    {C,R}.

listAdd(A,B,N, MinSleep, MaxSleep, Selective) ->
    CollectPID = self(),
    case Selective of
	true ->
	    LastPID = spawn(fun() -> worker:start(hd(A), hd(B), CollectPID, MinSleep, MaxSleep) end);
	false ->
	    LastPID = spawn(fun() -> worker:startSeq(hd(A), hd(B), CollectPID, MinSleep, MaxSleep) end)
    end,
    listAdd(tl(A), tl(B), N-1, LastPID, MinSleep, MaxSleep, Selective).

listAdd(_,_,0,PID, _,_,_) ->
    PID ! {[], [0]},
    receive
	{R,C} ->
	    {R,C}
    end;
listAdd(A,B,N,PID, MinSleep, MaxSleep, Selective) ->
    case Selective of
	true ->
	    LastPID = spawn(fun() -> worker:start(hd(A), hd(B), PID, MinSleep, MaxSleep) end);
	false ->
	    LastPID = spawn(fun() -> worker:startSeq(hd(A), hd(B), PID, MinSleep, MaxSleep) end)
    end,
    listAdd(tl(A), tl(B), N-1, LastPID, MinSleep, MaxSleep, Selective).

%% @doc prints L to the screen. Each integer in L will be checked against F. If F returns true, the integer will be printed. If F returns false a space will be printed, if none of the above is returned the result from F will be printed
-spec printList(L, F) -> ok when
      L::[integer()],
      F::fun().
				
printList([], _) ->
    io:format("~n");
printList([H|T], F) ->
    case F(H) of
	true ->
	    io:format("~B", [H]);
	false ->
	    io:format(" ");
	R -> 
	    io:format("~c", [R])
    end,
    printList(T, F).

%% @doc prints C, N times
-spec printMult(C, N) -> ok when
      C::char(),
      N::integer().
				
printMult(_, 0) ->
    ok;
printMult(C, N) ->
    io:format("~c", [C]),
    printMult(C, N-1).

%% @doc prints the lists as if they were used in a addition, where C is the carry, A is the frist number, B is the second number and R is the result from the addition.
-spec print(C,A,B,R) -> ok when
      C::[integer()],
      A::[integer()],
      B::[integer()],
      R::[integer()].
print(C, A, B, R) ->
    io:format(" "), printList([0]++tl(C), fun(I) -> I =:= 1 end),
    
    io:format(" "), printList([0]++tl(C), fun(I) -> 
			 (case I =:= 1 of
			     true ->
				 $-;
			     false->
				 false
			 end)
		 end),
    
    printMult(32, lists:max([0]++[length(B)-length(A)])+2), printList(A, fun(_) -> true end),
    
    printMult(32, lists:max([0]++[length(A)-length(B)])+2), printList(B, fun(_) -> true end),
    
    case hd(C) =:= 0 of
	true ->
	    io:format("+ "), printList(R, fun(_) -> $- end),
	    io:format(" ");
	false -> 
	    io:format("+"), printList(R, fun(_) -> $- end)
    end,
    io:format(" "), printList(R, fun(_) -> true end).

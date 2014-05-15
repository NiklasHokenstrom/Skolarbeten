%% @doc Functions for add module
-module(special).
-export([fill_list/2, print_result/4, add/3, add/4, 
	 integerToListBase/2, workerSpawner/6, sendPIDs/1, collect/2]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

%%-compile(export_all). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds Difference zeros to List
add_zero(List, 0) ->
    List;
add_zero(List, Difference) ->
    add_zero([0|List], Difference-1).

%% @doc If the lists List1 and List2 are of different lengths, add zeros
%% to the shorter list so that the resulting ListP and ListQ are of
%% equal length.
%% <div class="example">```
%% fill_list([1,2,3,4,5,6,7],[1,2,3]).
%% {[1,2,3,4,5,6,7],[0,0,0,0,1,2,3]}'''
%% </div>
-spec fill_list(List1, List2) -> {ListP, ListQ} when
      List1 :: [integer()],
      List2 :: [integer()],
      ListP :: [integer()],
      ListQ :: [integer()].

fill_list(L1, L2) ->
    Length1 = erlang:length(L1),
    Length2 = erlang:length(L2),
    Difference = Length1 - Length2,
    if
	Difference > 0 ->
	    {L1, add_zero(L2, Difference)};
	true ->
	    {add_zero(L1, -Difference), L2}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
%% @doc Prints Carry, A,B and Result so that it'll be easier to understand 
%% how the calculation was executed.
%% === Example ===
%% <div class="example">```
%% 1> special:print_result([0,1,0,0],[2,9,1,0],[3,5,0,0],[0,6,4,1,0]).
%%    1  
%%    -  
%%    2910
%%    3500
%% + -----
%%    6410
%% ok'''
%% </div>
-spec print_result(Carry,A,B,Result) -> ok when
      Carry::[integer()],
      A::[integer()],
      B::[integer()],
      Result::[integer()].
      
print_result(Carry, A, B, Result) ->
    io:format("  " ++ remove_all_zeros(int_list_to_string(Carry))++"~n"),
    io:format("  " ++ create_carry_line(Carry)++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(A))++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(B))++"~n"),
    io:format("+ " ++ line(length(Result))++"~n"),
    io:format("  " ++ remove_first_zeros(int_list_to_string(Result))++"~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates a list of '-' characters with size N.
line(N) ->
    utils:repeat($-, N).

%% Replaces all zeros in List with spaces.
remove_all_zeros(List) ->
    F = fun(X) when X =:= 48 -> X-16; (X) -> X  end,
    lists:map(F,List).

%% Copy List but replaces all 1:s with 45 (ASCII for '-'). 
create_carry_line(List) ->
    F = fun(X) when X =:= 1 -> 45; (_X) -> 32  end,
    lists:map(F,List).

%% Replaces the zeros in the beginning of List with spaces.
remove_first_zeros([]) ->
    [];
remove_first_zeros([48|T]) ->
    [32|remove_first_zeros(T)];
remove_first_zeros(List) ->
    List.

%% Converts a list with integers to corresponding string    
int_list_to_string(List) ->
    F = fun(X) when X < 10 -> X+48; (X) -> X+55 end,
    lists:map(F,List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns {ResultList, CarrierList} where ResultList is 
%% the result of A added to B in base Base. CarrierList 
%% contains a list of every of 1's and 0's where the 1's 
%% represents if a carry in has been made and 0 if a
%% carry in has not been made.
%%
%% === Example ===
%% <div class="example">```
%% 1> special:add([1,0,1],[1,1,1], 2).
%% {[1,1,0,0],[1,1,1,0]}'''
%% </div>
-spec add(A, B, Base) -> {ResultList, CarrierList} when
      A :: [integer()],
      B :: [integer()],
      Base :: integer(),
      ResultList :: [integer()],
      CarrierList :: [integer()].

add(A, B, Base) ->
    add(lists:reverse(A), lists:reverse(B), Base, 0, [], []).

%% @doc The same as add/3, but with an available Carry (carry-in).
%% === Example ===
%% <div class="example">```
%% 1> special:add([1,2,3],[0,9,7], 10, 1).
%% {[2,2,1],[1,1,1]}'''
%% </div>
-spec add(A, B, Base, Carry) -> {ResultList, CarrierList} when
      A :: [integer()],
      B :: [integer()],
      Base :: integer(),
      Carry :: integer(),
      ResultList :: [integer()],
      CarrierList :: [integer()].

add(A, B, Base, Carry) ->
    add(lists:reverse(A), lists:reverse(B), Base, Carry, [], []).

%% End of recursive add, 
%% the carry is added to the result, and the list of carries
add([],[], _, Carry, ResultList, CarrierList) ->
    {[Carry|ResultList],  [Carry|CarrierList]};

%% Recursive add where the heads A1 and B1 are added together.
%% (A1+B1+Carry) are consed to ResultList in the correct base 
%% and if there is a carry-out it'll be added to the CarrierList.
add([A1|A], [B1|B], Base, Carry, ResultList, CarrierList) ->
    NewCarry = (A1 + B1 + Carry) div Base,
    NewResult = (A1 + B1 + Carry) rem Base,
    add(A, B, Base, NewCarry, [NewResult|ResultList], [Carry|CarrierList]).

%% @doc Returns List where List is a representation of Number
%% in Base divided into elements.
%%
%% === Example ===
%% 
%% <div class="example">```
%% 1> special:integerToListBase(127,3).
%% [1,1,2,0,1]
%% 2> special:integerToListBase(190, 16).
%% [11,14]'''
%% </div>
integerToListBase(Number, Base) ->
    integerToListBase(Number, Base, []).

integerToListBase(0 , _Base, List) ->
    List;
integerToListBase(Number, Base, List) ->
    integerToListBase(Number div Base ,Base , [Number rem Base | List]).

%% Worker function for separate additions
%% worker({ListA,ListB}, CollectPID, Base) ->
%%     receive
%% 	{first, PID} ->
%% 	    {ResultList, CarryList} = add(ListA, ListB, Base),
%% 	    CollectPID ! {1, {ResultList, CarryList}},
%% 	    [CarryToSend|_] = CarryList,
%% 	    PID ! {carry, CarryToSend};
%% 	{alone} ->
%% 	    Result = {_ResultList, _CarryList} = add(ListA, ListB, Base),
%% 	    CollectPID ! Result;
%% 	{last, Pos} ->
%% 	    receive 
%% 		{carry, Carry} ->
%% 		    {ResultList, CarryList} = add(ListA, ListB, Base, Carry),
%% 		    CollectPID ! {Pos, {ResultList, CarryList}}
			
%% 	    end;
%% 	{carry, Carry} ->
%% 	    {ResultList, CarryList} = add(ListA, ListB, Base, Carry),
%% 	    receive
%% 		{pid, PID, Pos} ->
%% 		    [CarryToSend|_] = CarryList,
%% 		    PID ! {carry, CarryToSend},
%% 		    CollectPID ! {Pos, {ResultList, CarryList}}
%% 	    end
%%     end.

randomSleep(Min, Max) ->
    {A, B ,C} = now(),
    random:seed(A,B,C),
    timer:sleep(Min + random:uniform(Max-Min)).

worker2(ListA, ListB, Base, 0, ReturnPID,{Min, Max}) ->
    randomSleep(Min, Max),
    ReturnPID ! {case0, add(ListA, ListB, Base, 0)};
worker2(ListA, ListB, Base, 1, ReturnPID,{Min, Max}) ->
    randomSleep(Min, Max),
    ReturnPID ! {case1, add(ListA, ListB, Base, 1)}.


worker({ListA,ListB}, CollectPID, Base, {Min, Max}) ->
    receive
	{first, PID} ->
            randomSleep(Min, Max),
	    {ResultList, CarryList} = add(ListA, ListB, Base),
	    CollectPID ! {1, {ResultList, CarryList}},
	    [CarryToSend|_] = CarryList,
	    PID ! {carry, CarryToSend};
	{alone} ->
            randomSleep(Min, Max),
	    Result = {_ResultList, _CarryList} = add(ListA, ListB, Base),
	    CollectPID ! Result;
	{last, Pos} ->
            ReturnPID = self(),
            [Case0, Case1] = [spawn_link(fun()-> worker2(ListA, ListB, Base, C, ReturnPID,{Min, Max}) end) || C <- [0,1]],
	    receive 
		{carry, Carry} ->
                    if
                        Carry =:= 0 -> 
                            receive 
                                {case0, Result0} ->
                                    exit(Case1, not_needed),
                                    {ResultList, CarryList} = Result0
                            end;
                        true -> 
                            receive
                                {case1, Result1} ->
                                    exit(Case0, not_needed),
                                    {ResultList, CarryList} = Result1
                            end
                    end,
		    CollectPID ! {Pos, {ResultList, CarryList}}
			
	    end;
        {middle, Pos} ->
            ReturnPID = self(),
            [Case0, Case1] = [spawn_link(fun()-> worker2(ListA, ListB, Base, C, ReturnPID, {Min, Max}) end) || C <- [0,1]],
            receive
                {carry, Carry} ->
                    if
                        Carry =:= 0 -> 
                            receive 
                                {case0, Result0} ->
                                    exit(Case1, not_needed),
                                    {ResultList, CarryList} = Result0
                            end;
                        true -> 
                            receive
                                {case1, Result1} ->
                                    exit(Case0, not_needed),
                                    {ResultList, CarryList} = Result1
                            end
                    end
            end,
            receive
                {pid, PID, Pos} ->
                    [CarryToSend|_] = CarryList,
                    PID ! {carry, CarryToSend},
                    CollectPID ! {Pos, {ResultList, CarryList}}
            end
    end.

%% Recursively merges the results and carries from all the workers
merge([L], ResultList, CarryList) ->
    %% we do care about the head in the base case, 
    %% because this is the (msb)carryover.
    {_,{Result, Carry}} = L, 
    {Result ++ ResultList, Carry ++ CarryList};
merge([L1|L],ResultList, CarryList) ->
    %% the head is the carryover so when we merge we dont want it here
    {_,{[_  | Result], [_ | Carry]}} = L1, 
    merge(L, Result ++ ResultList, Carry ++ CarryList).

%% @doc Recursively collects N results from the N workers and merges their 
%% ResultLists and CarrierLists to a final result of the additions.
%%
%% === Example ===
%% <div class="example">```
%% 1> special:collect(3,[{2,{[4,2,9],[1,0,1]}}]).
%% ...
%% ...(collects from all the other workers recursively)...
%% ...
%% {[3,7,0,9,4,2,9,3,2,1],[0,0,1,0,1,0,1,0,0,0]}'''
%% </div>
-spec collect(N, AuxList) -> {ResultList, CarrierList} when
      N :: integer(),
      AuxList :: [{Pos, {ResultList, CarrierList}}],
      Pos :: integer(),
      ResultList :: [integer()],
      CarrierList :: [integer()].

collect(0, AuxList) ->
    io:format("~p~n~n", [AuxList]),
    merge(lists:keysort(1, AuxList),[],[]);
collect(N, AuxList) ->
    receive
	{Pos ,{ResList, CarList}} ->
	    collect(N-1, [{Pos, {ResList, CarList}} | AuxList]);
	{ResList, CarList} -> %% Alone
	    {ResList, CarList}
    end.

%% @doc Recursively spawns the neccessary workers to calculate the additions.
%% 
%% === Example ===
%% <div class="example">```
%% 1> special:workerSpawner(utils:split([1,2,3,4],2),utils:split([1,2,3,4],2), [], self(), 10).
%% [<0.47.0>,<0.46.0>]'''
%% </div> 
-spec workerSpawner(A, B, AuxList, CollectPID, Base, MinMax) -> AuxList when
      A :: [integer()],
      B :: [integer()],
      AuxList :: [pid()],
      CollectPID :: pid(),
      Base :: integer(),
      MinMax:: tuple().

workerSpawner([],[], Aux, _, _,_) ->
    Aux;
workerSpawner([HeadA |ListA], [HeadB |ListB], Aux, CollectPID, Base, MinMax) ->
    workerSpawner(ListA, ListB, [spawn_link(fun()-> worker({HeadA, HeadB}, CollectPID, Base, MinMax) end) | Aux], CollectPID, Base, MinMax).

%% @doc Recursively signals all the workers the with PID of the next worker, so
%% that the carry-outs can be sent to the correct worker. The first worker
%% will know it's first, so we start with position 2.
%% 
%% === Example ===
%% <div class="example">```
%% 1> special:sendPIDs([<0.58.0>, <0.59.0>, <0.60.0>]).
%% ok'''
%% </div>
-spec sendPIDs(PIDList) -> ok when
      PIDList :: [pid()].

sendPIDs(List) ->
    sendPIDs(List, 2).

sendPIDs(L, Pos) when length(L) =:= 1 ->
    [SendPID] = L,
    SendPID ! {last, Pos};

sendPIDs([SendPID|[RecievePID|L]], Pos) ->
    SendPID ! {middle, Pos},
    SendPID ! {pid, RecievePID, Pos},
    sendPIDs([RecievePID|L], Pos + 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_all_zeros_test_() ->
    ?_assertEqual({" 1 2 3 ", "  1  9  "}, 
		  {remove_all_zeros("0102030"), 
		  remove_all_zeros("00100900")}).

remove_first_zeros_test_() ->
    ?_assertEqual({"  12030", "123"}, 
		  {remove_first_zeros("0012030"),
		  remove_first_zeros("123")}).

int_list_to_string_test_() ->
    ?_assertEqual({"98710", "00123"}, 
		  {int_list_to_string([9,8,7,1,0]), 
		   int_list_to_string([0,0,1,2,3])}).

add_zero_test_() ->
    ?_assertEqual({[0,0,0,1], [0,0,5,2,9]}, 
		  {add_zero([1],3), 
		   add_zero([5,2,9],2)}).

fill_list_test_() ->
    ?_assertEqual({{[1,2,3],[0,0,1]},
		   {[0,0,0,0,1,2,3,4,5,6],[9,8,7,6,5,4,3,2,1,0]}}, 
		  {fill_list([1,2,3],[1]),
		   fill_list([1,2,3,4,5,6],[9,8,7,6,5,4,3,2,1,0])}).

result_test_() ->
    Assert = fun(A,B,Base) ->
		     {Al,Bl} = fill_list(integerToListBase(A,Base),integerToListBase(B,Base)),
		     {Result,_Carry} = add(Al,Bl,Base),
		     ?_assertEqual(A+B,list_to_integer(int_list_to_string(Result),Base))
	     end, 
    random:seed(erlang:now()),
    [Assert(A,B,Base) || A <- [random:uniform(1000000) || _ <- lists:seq(1,4)],
			      B <- [random:uniform(1000000) || _ <- lists:seq(1,4)],
			      Base <- lists:seq(2,36)].

concurrent_result_test_() ->
    Assert = 
	fun(A,B,Base,N,MinMax) ->
		A1 = integerToListBase(A, Base),
		B1 = integerToListBase(B, Base),
		{A2, B2} = fill_list(A1, B1),
		SplitA = utils:split(A2, N),
		SplitB = utils:split(B2, N),
		CollectPID = self(),

		WorkerPids = workerSpawner(SplitA, SplitB, [], 
						   CollectPID, Base, MinMax),

		io:format("~p~n", [WorkerPids]),
		[FirstProcess|RestPids] = WorkerPids, 

		if
		    length(WorkerPids) =:= 1 ->
			FirstProcess ! {alone};
		    true -> 
			SecondProcess = lists:nth(2, WorkerPids),
			FirstProcess ! {first, SecondProcess},
			sendPIDs(RestPids)
		end,
		    {Result, _Carry} = collect(length(WorkerPids), []),
		?_assertEqual(A+B,list_to_integer(int_list_to_string(Result),Base))

	end,

    [Assert(A,B,Base,N,{0,100}) || A <- [random:uniform(1000000) || _ <- lists:seq(1,4)],
				   B <- [random:uniform(1000000) || _ <- lists:seq(1,4)],
				   Base <- lists:seq(2,16),
				   N <- lists:seq(2,4)].

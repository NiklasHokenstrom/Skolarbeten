%% @doc Erlang mini project.
-module(add).
-export([start/3, start/5]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

%% @doc Sequentially calculates the addition between A and B in base Base
%%
%% === Example ===
%% <div class="example">```
%% 1> add:start(478,679,16).
%%    11 
%%    -- 
%%    1DE
%%    2A7
%% + ----
%%    485
%%  ok'''
%% </div>
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) when Base >= 2 andalso Base =< 36 ->
    {Al,Bl} = special:fill_list(special:integerToListBase(A,Base),
				special:integerToListBase(B,Base)),
    {Result, Carry} = special:add(Al,Bl,Base),
    special:print_result(Carry, Al, Bl, Result).
    

%% @doc Concurrently calculates the addition between A and B in base Base,
%% divided over N workers. The integers A and B are split into segments
%% of equal (or close to equal) length/size and then sent to separate
%% workers that calculates their segments of A and B. Carry-overs can be
%% sent between workers and the workers individually sends their results
%% to the parent process that simply merges them into a final result.
%% 
%% === Example ===
%% <div class="example">```
%% 1> 17> add:start(4985581589776235987988, 6793124512341289768987, 16, 5).
%%        111    1111    
%%        ---    ----    
%%    10E44D7E9287BCF8414
%%    170418A60754D3E981B
%% + --------------------
%%    27E8662499DC90E1C2F
%% ok'''
%% </div>

-spec start(A,B,Base, Options, MinMax) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option],
      MinMax:: tuple().

start(A,B,Base,N, MinMax) ->
    A1 = special:integerToListBase(A, Base),
    B1 = special:integerToListBase(B, Base),
    {A2, B2} = special:fill_list(A1, B1),
    SplitA = utils:split(A2, N),
    SplitB = utils:split(B2, N),
    CollectPID = self(),

    WorkerPids = special:workerSpawner(SplitA, SplitB, [], 
				       CollectPID, Base, MinMax),
    
    io:format("~p~n", [WorkerPids]),
    [FirstProcess|RestPids] = WorkerPids, 

    if
	length(WorkerPids) =:= 1 ->
	    FirstProcess ! {alone};
	true -> 
	    SecondProcess = lists:nth(2, WorkerPids),
	    FirstProcess ! {first, SecondProcess},
	    special:sendPIDs(RestPids)
    end,
    
    
    {Result, Carry} = special:collect(length(WorkerPids), []),
    special:print_result(Carry, A2, B2, Result).

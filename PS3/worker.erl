-module(worker).
-compile(export_all).

%NOTE: super parent reffers to the process created when you call add:start
%NOTE: "middle process" reffers to a process created by super parent

%% @doc start(A,B,PID) will be called from add:listAdd.
%It is the starting function for the spawns from super parent

%A is a list with numbers ex:[1,2,3]

%B is also a list with numbers ex:[4,5,6]

%PID is the PID for the process that should recieve the result from this process.
%PID could either be super parent or another "middle process"
%If the following split were made:
%   [[1,2,3], [4,5,6]]
%Then 2 processes will be created and can be named liked this:
%   [A, B] where A works on list [1,2,3] and B on [4,5,6]
%The PIDs will then be as following:
%    As' PID will point to super parent
%    Bs' PID will point to A
-spec start(A,B,PID,MinSleep,MaxSleep) -> ok when
      A :: [integer()],
      B :: [integer()],
      PID :: pid(),
      MinSleep :: integer(),
      MaxSleep :: integer().

start(A,B,PID, MinSleep, MaxSleep) ->
    CollectPID = self(), %Store the current PID for use when spawning children
    random:seed(now()),
    SleepA = random:uniform((MaxSleep-MinSleep)+1)+MinSleep,
    SleepB = random:uniform((MaxSleep-MinSleep)+1)+MinSleep,
    spawn_link(fun() -> child(A, B, 0, CollectPID, SleepA) end), %Spawn a child and tell it to work on the addition A+B with 0 as carryIn
    spawn_link(fun() -> child(A, B, 1, CollectPID, SleepB) end), %Spawn a child and tell it to work on the addition A+B with 1 as carryIn
    loop([], PID, [], []). %All children have now been spawned so it is time to wait for a result coming from either the children or the previous "middle process" or super parent

%% @doc see start. starSeq only differs from start in the sece that it waits for a reply from middle process or super parent before creating a child.
-spec startSeq(A,B,PID,MinSleep,MaxSleep) -> ok when
      A :: [integer()],
      B :: [integer()],
      PID :: pid(),
      MinSleep :: integer(),
      MaxSleep :: integer().

startSeq(A,B,PID, MinSleep, MaxSleep) ->
    startSeq(A,B,PID,[],[], MinSleep, MaxSleep).
startSeq(A,B,PID, Rn, Cn, MinSleep, MaxSleep) ->
    receive
	{R,C} ->
	    CollectPID = self(),
	    random:seed(now()),
	    Sleep = random:uniform((MaxSleep-MinSleep)+1)+MinSleep,
	    spawn(fun() -> child(A,B,hd(C), CollectPID, Sleep) end),
	    startSeq(A,B,PID,R,C, MinSleep, MaxSleep);
	{R,C,_} ->
	    PID ! {R++Rn, C++tl(Cn)}
    end.

%% @doc loop(L, PID, Rn, Cn) will collect and pass on result from children.

%L is used as a buffer to collect the data from the children
%There will be at most 2 elements in L, one for each child(with/without carry)
%Each element in L will be a list with 3 elements.
%The first element will be the result-list from the calculation(NOTE:this is the sub-result and is not merged with the result of previous "middle processes")
%The second element will be the resulting carry-list from the calculation(NOTE:same as NOTE for first element)
%The third element will be an integer with the input carry.(NOTE: this is fixed to 1/0 by worker:start)

%PID is the same as the PID in worker:start

%Rn is used to save a result from previous "middle process" or super parent.
%We need to save Rn in case we get Rn before the children have finnished their work.

%Cn is used to save a carry-list from previous "middle process" or super parent.
%We need to save this for the same reasons as Rn.
%If the case is that our children are done before we get Cn, we only use Cn to determine which child did the correct addition.

-spec loop(L, PID, Rn, Cn) -> ok when
      L :: [List],
      PID :: pid(),
      Rn :: [integer()],
      Cn :: [integer()],
      List :: [T],
      T :: term().

loop(L, PID, Rn, Cn) ->
    receive
	{R,C,Nc} when length(L) =:= 1, length(Cn) =/= 0-> %This is a reply from a child. The guard check to see that the other child is done(resulting in all children being done) and if we already got a Cn from previous "middle process". NOTE: the check must be used on Cn, {Rn, Cn} from super parent looks like this: {[], [0]}. So if the check were done on Rn, the first middle process would never start
	    passResult([[R,C,Nc]]++L, PID, Rn, Cn); %If the above explained guard is true we can safely pass along the result. before passResult is called we concat the reply from the child with L buffer (See the comment on L in worker:loop)
	{R,C,Nc} -> %If the guard above said no, we would come here, meaning that either no child is done or no reply from middle user has been send. 
	    loop([[R,C,Nc]]++L, PID, Rn, Cn); %Simply add the child result to L buffer (See the comment on L in worker:loop)
	{R,C} when length(L) =:= 2 -> %This is a reply from a middle process or super parent. The guard makes sure that all the children are done working.
	    passResult(L, PID, R, C); %No work needs to be done, just pass along the data to passResult
	{R,C} -> %This means that we got a reply from middle process or super parent but our children aren't done, so we need to continue looping with the saved Rn, Cn
	    loop(L, PID, R, C) %Loop with the new Rn, Cn
    end.


%passResult(L, PID, R, C) is used to determine which child did the correct calculation, "clean up" the result and send it to the next middle process or super parent

%L see comment on L in worker:loop

%PID see comment on PID in worker:start

%R see comment on Rn in worker:loop

%C see comment on Cn in worker:loop

-spec passResult(L, PID, R, C) -> ok when
      L :: [list()],
      PID :: pid(),
      R :: [integer()],
      C :: [integer()].

passResult(L, PID, R, C) ->
    Pass = hd([[Rp, Cp, Nc] || [Rp, Cp, Nc] <- L, Nc =:= hd(C)]), %This match will give us a list from the child which carry in is the same as the first element in C. We use head just to get rid of the double list.
    PID ! {hd(Pass)++R, hd(tl(Pass))++tl(C)}. %Send the result to next middle process or super parent. The first element in the tuple will simply get the result list from Pass and concat it with R. The second argument is a bit more tricky. First of taking the head of the tail of Pass will result in the second argument in Pass. If a straightforward concat were made with C there will be one duplicate carry for each middle process(Either think about it, or just trust me and call it black magic), so we use concat with the tail of C to remove this problem.


%child(A,B,Nc, PID)

%A see comment on A in worker:start

%B see comment on B in worker:start

%Nc is the carryIn for the addition A+B, so A+B+Nc

%PID see comment on PID in worker:start

-spec child(A, B, Nc, PID) -> ok when
      A :: [integer()],
      B :: [integer()],
      Nc :: integer(),
      PID :: pid().

child(A,B,Nc, PID,Sleep) ->
    timer:sleep(Sleep),
    child(A,B,Nc, PID).
child(A,B,Nc, PID) ->
    {R, C} = utils:add(A,B,[Nc]), %Call utils:add and match out the result into {R,C}
    PID ! {R, C, Nc}. %Send the result to middle process.

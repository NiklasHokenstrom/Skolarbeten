
%% @author Karl Marklund <karl.marklund@it.uu.se>

%% @doc A small collection of utility functions. 


-module(utils). 

%-export([seqs/1, filter/2, split/2, split/4, delete/2]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 


%% @doc Generates a list of lists of increasing sequences of integers
%% starting with the empty list and ending with [1,2, ..., N].
%% === Example ===
%% <div class="example">```
%% > utils:seqs(5).
%% [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]'''
%% </div>
-spec seqs(N::integer()) -> [[integer()]].

seqs(N) ->
    %% NOTE: Simply using a list comprehension such as [[]] ++
    %% [lists:seq(1,M) || M <- lists:seq(1,N)] will be quite slow
    %% since each sequence is generated from scratch. Hence, lets
    %% re-use the last sequnece and add a new element when
    %% constructing the next sequence.
    
    F = fun(X,[H|T]) -> [[X|H],H|T] end,
    lists:foldl(F, [[]], lists:seq(1,N)),
    lists:reverse([lists:reverse(L) || L <- lists:foldl(F, [[]], lists:seq(1,N))]).

		
%% @doc Each list in List2 contains the elements Elem in List1 for
%% which one of the Pred(Elem) returns true. The order of the lists in
%% List2 is the same as the order of the predicates. In each list in
%% List2, the relative order of the elements are the same as in the
%% original List1. 
%% 
%% === Example ===
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> P1 = fun(X) -> X rem 2 == 1 end.
%% #Fun<erl_eval.6.111823515>  
%% 3> P2 = fun(X) -> not P1(X) end. 
%% #Fun<erl_eval.6.111823515>
%% 4> P3 = fun(X) -> X > 3 andalso X < 7 end. 
%% #Fun<erl_eval.6.111823515>
%% 5> utils:filter([P1,P2,P3], L).
%% [[1,3,5,7,9],[2,4,6,8,10],[4,5,6]]'''
%% </div>
-spec filter(Preds, List1) -> List2 when
      Preds :: [Pred],
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [[T]],
      T :: term().

filter(Predicates, List) ->
    Collect = self(),
    [spawn(fun() -> Collect!{I,lists:filter(P,List)} end) ||
	{I, P} <- lists:zip(lists:seq(1, length(Predicates)), Predicates)],
    
    filter_collect(length(Predicates), []).

filter_collect(0,R) ->
    [L || {_,L} <- lists:sort(R)];
filter_collect(N,R) ->
    receive
	{I, L} -> filter_collect(N-1, [{I,L}|R])
    end.



lqr(L, N) ->
    Len = length(L),

    %% Quotient
    Q = Len div N, 
    
    %% Reminder
    R = Len rem N, 
    
    {Len, Q, R}. 

%% @doc Split List into N Lists such that all Lists have approximately the same number of elements.  
%% 
%% Let Len = length(List), Q = Len div N and R = Len rem N. 
%% 
%% If R = 0, then all of the lists in Lists will be of length Q. 
%% 
%% If R =/= 0, then R of the lists in Lists will have
%% lenght Q + 1. 
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> utils:split(L, 4).
%% [[1,2],[3,4],[5,6,7],[8,9,10]]
%% 3> lists:concat(utils:split(L,3)).
%% [1,2,3,4,5,6,7,8,9,10]'''
%% </div>
-spec split(List, N) -> Lists when
      List :: [T],
      Lists :: [List],
      T :: term(),
      N :: integer().


split(L, N) ->
    {_, Q, R} = lqr(L, N),
    split(L, [], Q, R).
split([], SegList, _, _) ->
    SegList;
split(L, SegList, Q, R) -> 
    case R > 0 of
	true -> 
	    Segment = [lists:sublist(L, 1, Q+1)],
	    NewList = delete(L, Q+1),
	    Rtmp = R - 1,
	    split(NewList, SegList ++ Segment, Q, Rtmp);
	false ->
	    Segment = [lists:sublist(L, 1, Q)],
	    NewList = delete(L, Q),
	    split(NewList, SegList ++ Segment, Q, 0)
    end.

expandList([0], L) ->
    L;
expandList([N], L) ->
    expandList([N div 10], [N rem 10] ++ L).

%% @doc Removes the N first elements from L. Returns the new List.

-spec delete(L, N) -> List when
      List :: [T],
      L :: [T],
      N :: integer(),
      T :: term().
    
delete(L, 0) ->
    L;
delete([_|T], N) -> 
    delete(T, N-1).

%% @doc Adds A and B, and concatinates every carry out to C, and then uses the head of C as carry in.
%%  returns a tuple with the sum of A and B, and the list of carries.

-spec add(A, B, C) -> {Sum, CarryList} when
      A :: [T],
      B :: [T],
      C :: [T],
      Sum :: [T],
      CarryList :: [T],
      T :: integer().

add(A, B, C) ->
    Arev = lists:reverse(A),
    Brev = lists:reverse(B),
    add(Arev, Brev, C, []).
add([], [], C, Sum) ->
    {Sum, C};
add(A, B, C, Sum) ->
    
    NewSum = [((hd(A) + hd(B) +hd(C)) rem 10)] ++ Sum,

    case ((hd(A) + hd(B) + hd(C)) > 9) of
	true -> 
	    NewC = [1] ++ C;
	false ->
	    NewC = [0] ++ C
    end,
        
    add(tl(A), tl(B), NewC, NewSum).



% @doc Converts Number from base 10 to desired base,
% returns converted number as string.
-spec convertFromTen(ToBase, Number) -> Result when
      ToBase :: string(),
      Number :: integer(),
      Result :: [T],
      T :: term().

convertFromTen(ToBase, Number) ->
   io_lib:format("~."++ToBase++"B", [Number]).

% @doc Converts Number from specified base to base 10,
% returns converted number as string list.
-spec convertToTen(FromBase, Number) -> Result when
      FromBase :: string(),
      Number :: string(),
      Result :: [T],
      T :: term().

convertToTen(FromBase, Number) ->   
    {ok, [R], _} = io_lib:fread("~"++FromBase++"u", Number),
    [R].


% @doc Fills the shortest list of A and B with zero, intill they have the same length.
% returns a tuple with one new and one old list

-spec addZeroToShortest(A, B) -> {List1, List2} when
      A :: [T],
      B :: [T],
      List1 :: [T],
      List2 :: [T],
      T :: term().
      
addZeroToShortest(A, B) ->
    Difference = length(A) - length(B),
    addZeroToShortest(A, B, Difference).
addZeroToShortest(A, B, 0) ->
    {A, B};
addZeroToShortest(A, B, Difference) when length(A) > length(B) ->
    addZeroToShortest(A, [0]++B, Difference-1);
addZeroToShortest(A, B, Difference) ->
    addZeroToShortest([0]++A, B, Difference+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seqs_length_test_() ->
    %% The list [[], [1], [1,2], ..., [1,2, ..., N]] will allways have
    %% length N+1.

    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

seqs_test_() ->
    %% A small collection of expected results {N, seqs(N)}.
    
    Data = [{0, [[]]}, {1, [[], [1]]}, {2, [[], [1], [1,2]]}, 
	    {7, [[],
		 [1],
		 [1,2],
		 [1,2,3],
		 [1,2,3,4],
		 [1,2,3,4,5],
		 [1,2,3,4,5,6],
		 [1,2,3,4,5,6,7]]}
	   ],
    
    [?_assertEqual(L, seqs(N)) || {N, L} <- Data].
    
filter_test_() ->
    [?_assertEqual([], filter([], L)) || L <- seqs(10)].
    
filter_true_false_test_() ->
    P1 = fun(_) -> false end,
    P2 = fun(_) -> true end,
    P3 = fun(X) -> X rem 2 == 0 end,
    
    Expected = fun(L) -> [lists:filter(P,L) || P <- [P1,P2,P3]] end,

    [?_assertEqual(Expected(L), filter([P1,P2,P3], L) ) || L <- seqs(10) ].
				       
filter_test() ->
    L = lists:seq(1,10),

    P1 = fun(X) -> X rem 2 == 0 end,
    P2 = fun(X) -> X rem 2 == 1 end,
    P3 = fun(X) -> X > 3 end,

    %%E = [[2,4,6,8,10],[1,3,5,7,9],[4,5,6,7,8,9,10]],
    E = [lists:filter(P,L) || P <- [P1,P2,P3]],
    
    ?assertEqual(E, filter([P1,P2,P3], L)).
    
split_concat_test_() ->
    %% Make sure the result of concatenating the sublists equals the
    %% original list.
    
    L = lists:seq(1,99),
    [?_assertEqual(L, lists:concat(split(L,N))) || N <- lists:seq(1,133)].

split_n_test_() ->
    %% Make sure the correct number of sublists are generated. 
    
    M = 99,
    L = lists:seq(1,M),
    Num_of_lists = fun(List, N) when N =< length(List) ->
			   N;
		      (List, _) ->
			   length(List)
		   end,
    [?_assertEqual(Num_of_lists(L,N), length(split(L,N))) || N <- L].    


expected_stat(L, N) when N =< length(L) ->
    %% When spliting a list L into N sublists, we know there will only by two possible
    %% lengths of the sublists.

    
    %% Quotient and reminder when dividing length of L with N. 
    {_, Q, R} = lqr(L, N),

    %% There will allways be R sublists of length Q+1 and N-R sublists
    %% of length Q.
    
    {{R, Q+1}, {N-R, Q}};

expected_stat(L, _N) ->
    %% N greater than the length of L, hence all sublists will have
    %% length 1.

    {{length(L), 1}, {0,0}}.

stat(N, M, LL) ->
    %% Return a tuple {{Num_N, N}, {Num_M, M}} where Num_N is the
    %% number of lists of length N in LL and Num_M is the number of
    %% lists of length M in LL.
    
    S = filter([fun(X) -> X == N end, fun(X) -> X == M end], [length(L) || L <- LL]),

    [Num_N, Num_M] = [length(L) || L <- S],
    
    {{Num_N, N}, {Num_M, M}}.

split_stat_test_() ->
    %% Assure the list of sublists contains the correct number of
    %% lists of the two expected lengths.
	
    Assert = fun(L,N) ->
		     {_, Q, _} = lqr(L,N), 
		     ?_assertEqual(expected_stat(L,N), stat(Q+1, Q, split(L,N))) 
	     end,
	
    %% Generators can depend on other generator expressions, here N
    %% depends on the length of L.
    
    [Assert(L,N) ||  L <- seqs(33), N <- lists:seq(1,length(L)+5)].
    
add_test_() ->


    A = lists:seq(1,3),
    B = lists:seq(1,3),

    C = lists:seq(1,7,3),
    D = lists:seq(1,7,3),

    E = [9,9,9],
    % Tests with addition which generates not carry outs, only takes one carry in from the carrylist.
    [?_assertEqual({[2,4,6],[0,0,0,0]}, add(A, B, [0])),
     ?_assertEqual({[2,4,7],[0,0,0,1]}, add(A, B, [1])),
     ?_assertEqual({[2,4,6],[0,0,0,0,1,1,0]}, add(A, B, [0,1,1,0])),
     ?_assertEqual({[2,4,7],[0,0,0,1,1,0,1]}, add(A, B, [1,1,0,1])),
     % Tests when carry outs are generated, with and without carry in from the list
     ?_assertEqual({[2,9,4],[0,0,1,0]}, add(C, D, [0])),
     ?_assertEqual({[2,9,5],[0,0,1,1]}, add(C, D, [1])),
     ?_assertEqual({[2,9,4],[0,0,1,0,1,1,0]}, add(C, D, [0,1,1,0])),
     ?_assertEqual({[2,9,5],[0,0,1,1,1,0,1]}, add(C, D, [1,1,0,1])),
     % Tests when there is always a carry out generated, with and without carry in from the list.
     ?_assertEqual({[9,9,8],[1,1,1,0]}, add(E, E, [0])),
     ?_assertEqual({[9,9,9],[1,1,1,1]}, add(E, E, [1])),
     ?_assertEqual({[9,9,8],[1,1,1,0,1,1,0]}, add(E, E, [0,1,1,0])),
     ?_assertEqual({[9,9,9],[1,1,1,1,1,0,1]}, add(E, E, [1,1,0,1]))
    ].

convertFromTen_test_() ->
    [?_assert(convertFromTen("2", 10) =:= ["1010"]),
     ?_assert(convertFromTen("10", 10) =:= ["10"]),
     ?_assert(convertFromTen("16", 10) =:= ["A"]),
     ?_assert(convertFromTen("8", 10) =:= ["12"])].

convertToTen_test_() ->
    [?_assert(convertToTen("10", "10") =:= [10]),
     ?_assert(convertToTen("2", "1010") =:= [10]),
     ?_assert(convertToTen("16", "A") =:= [10]),
     ?_assert(convertToTen("8", "12") =:= [10])].

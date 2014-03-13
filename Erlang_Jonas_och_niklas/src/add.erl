%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B,Base) ->
    utils:print_result(A,B,Base).
    %%Pos = (A+B) div Base,
    %%io:format(utils:repeat($ ,Pos + 3) ++ integer_to_list(A,Base) ++ "~n"),
    %%io:format(utils:repeat($ ,Pos + 3) ++ integer_to_list(B,Base) ++ "~n"),
    %%io:format("+ " ++ utils:repeat($-, ((A+B) div Base) + 2) ++ "~n"),
    %%io:format(utils:repeat($ ,Pos + 3) ++ integer_to_list(A + B,Base) ++ "~n").


%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, {N}) ->
    Al = [Ai || {Ai,_} <- lists:map(fun(X) -> string:to_integer(X) end,utils:split(integer_to_list(A),N))],
    Bl = [Bi || {Bi,_} <- lists:map(fun(X) -> string:to_integer(X) end,utils:split(integer_to_list(B),N))],
    Result = utils:add_lists(Al,Bl),
    io:format(lists:concat(lists:map(fun(X) -> integer_to_list(X) end,Result)) ++ "~n").
    


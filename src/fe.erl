-module(fe).

-export([papply/2]).
-export([all/1, any/1]).
-export([count/2, uniq/1]).
-export([true/0, false/0, id/1]).

-type predicate() :: fun(() -> boolean()).

%% =====================================================================                
%% Composition 
%% =====================================================================                
-spec papply(Fun::fun(), Fix::any()) -> fun().
papply(Fun, Fix) -> 
    case arity(Fun) of
        0 ->
            {error, badarity};
        1 -> 
            fun() -> apply(Fun, [Fix]) end;
        2 -> 
            fun(Arg) -> apply(Fun, [Fix|[Arg]]) end;
        _ -> 
            fun(Args) when is_list(Args) -> apply(Fun, [Fix|Args]) end
    end.

%% =====================================================================                
%% Logics                                                                               
%% =====================================================================                

-spec all(Preds::[predicate()]) -> predicate().
all(Preds) when is_list(Preds) ->
    fun() -> lists:foldl(fun(Pred, Acc) -> Acc and Pred() end, true, Preds) end.

-spec any(Preds::[predicate()]) -> predicate().
any(Preds) when	is_list(Preds) ->
    fun() -> lists:foldl(fun(Pred, Acc) -> Acc or Pred() end, false, Preds) end.

%% =====================================================================                
%% Collections
%% =====================================================================                

-spec count(Needle::any(), Haystack::[any()]) -> non_neg_integer().
count(Needle, Haystack) ->
    lists:foldl(fun(N, Count) when N =:= Needle -> Count + 1;
                   (_, Count) -> Count end, 0, Haystack).

-spec uniq(List::list()) -> list().
uniq(List) ->
    lists:usort(List).

%% =====================================================================                
%% Utility
%% =====================================================================                
-spec true() -> true.
true() -> true.

-spec false() -> false.
false() -> false.

-spec id(Any::any()) -> any().
id(Any) -> Any.

arity(Fun) -> 
    {arity, N} = erlang:fun_info(Fun, arity),
    N.

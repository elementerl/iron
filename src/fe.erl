-module(fe).

-export([bind/2]).
-export([all/1, any/1]).
-export([true/0, false/0, id/1]).
-export([count/2, uniq/1, foldl1/2]).

-type predicate() :: fun(() -> boolean()).

%% =====================================================================
%% Composition
%% =====================================================================
bind(Arg, Fun) -> fun() -> apply(Fun, [Arg]) end.

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

-spec uniq(list()) -> list().
uniq(List) ->
    lists:usort(List).

-spec foldl1(fun((Element::any(), Acc::any()) -> Acc::any()), list()) -> Acc::any().
foldl1(Fun, [X|Rest]) ->
    lists:foldl(Fun, X, Rest).

%% =====================================================================
%% Utility
%% =====================================================================
-spec true() -> true.
true() -> true.

-spec false() -> false.
false() -> false.

-spec id(any()) -> any().
id(Any) -> Any.

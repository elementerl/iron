-module(fe).

-export([true/0, false/0]).
-export([all/1, any/1]).
-export([count/2]).

-type predicate() :: fun(() -> boolean()).

-spec true() -> true.
true() -> true.

-spec false() -> false.
false() -> false.

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
%% Tabulations                                                                          
%% =====================================================================                

-spec count(Needle::any(), Haystack::[any()]) -> non_neg_integer().
count(Needle, Haystack) ->
    lists:foldl(fun(N, Count) when N =:= Needle -> Count + 1;
                   (_, Count) -> Count end, 0, Haystack).

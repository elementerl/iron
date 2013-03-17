-module(fe_tests).
-include_lib("eunit/include/eunit.hrl").

%% Composition tests
bind_test_() ->
    Bound = fe:bind(true, fun fe:id/1),
    { "binds an argument to a function", ?_assertMatch(true, Bound()) }.

%% Logics tests
all_test_() ->
    AllT = fe:all([fun fe:true/0, fun fe:true/0]),
    OneF = fe:all([fun fe:true/0, fun fe:false/0]),

    [
        { "all true means the composite is true",
            ?_assertMatch(true, AllT()) },

        { "one false means the composite is false",
            ?_assertMatch(false, OneF()) }
    ].

any_test_() ->
    AnyT = fe:any([fun fe:true/0, fun fe:false/0]),
    None = fe:any([fun fe:false/0, fun fe:false/0]),

    [
        { "true if any are true", ?_assertMatch(true, AnyT()) },
        { "false if none are true", ?_assertMatch(false, None()) }
    ].

%% Collections tests
count_test_() ->
    { "returns the count of needles found in the haystack", 
        ?_assertMatch(2, fe:count(2, [1,4,2,4,2])) }.

uniq_test_() ->
    { "returns the ordered set of uniq items from a list",
        ?_assertMatch([1,2,3], fe:uniq([2,1,1,3,3,2])) }.

%% Utility tests
true_test() -> true = fe:true().
false_test() -> false = fe:false().

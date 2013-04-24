-module(fe_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

-define(PROP(P), ?_assertMatch(true, triq:check(P))).

%% Composition tests
papply_test_() ->
    PA = fe:papply(fun fe:id/1, true),
    N1 = fe:papply(fun fe:count/2, foo),
    NM = fe:papply(fun(A,B,C) -> A + B + C end, 2),
    [
        { "reducing arity from 1 to 0",
            ?_assertMatch(true, PA()) },

        { "reducing arity from N to 1",
            ?_assertMatch(3, N1([bar, foo, foo, baz, foo])) },

        { "reducing arity from N to M",
            ?_assertMatch(9, NM([3,4])) },

        { "applying a 0-arity fun is an error",
            ?_assertMatch({error, badarity}, fe:papply(fun() -> ok end, wat)) }
    ].

%% Logics tests

pnot_test_() ->
    PTrue  = fun() -> true end,
    PFalse = fun() -> false end,

    [
     { "true predicates becomes false",
       ?_assertMatch(false, (fe:pnot(PTrue))())},

     { "false predicates becomes true",
       ?_assertMatch(true, (fe:pnot(PFalse))())}
    ].

pand_test_() ->
    PTrue  = fun() -> true end,
    PFalse = fun() -> false end,

    [
     { "true && true predicates are true",
       ?_assertMatch(true, (fe:pand(PTrue, PTrue))())},

     { "false && true predicates are false",
       ?_assertMatch(false, (fe:pand(PFalse, PTrue))())},

     { "true && false predicates are false",
       ?_assertMatch(false, (fe:pand(PTrue, PFalse))())},

     { "false && false predicates are false",
       ?_assertMatch(false, (fe:pand(PFalse, PFalse))())}
    ].

por_test_() ->
    PTrue  = fun() -> true end,
    PFalse = fun() -> false end,

    [
     { "true && true predicates are true",
       ?_assertMatch(true, (fe:por(PTrue, PTrue))())},

     { "false && true predicates are true",
       ?_assertMatch(true, (fe:por(PFalse, PTrue))())},

     { "true && false predicates are true",
       ?_assertMatch(true, (fe:por(PTrue, PFalse))())},

     { "false && false predicates are false",
       ?_assertMatch(false, (fe:por(PFalse, PFalse))())}
    ].

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

%% Tuples

fst_test_() ->
    [
     { "be a synonym for erlang:element/2 for non-empty tuples",
       ?PROP(?FORALL(T, tuple(int()), ?IMPLIES(tuple_size(T) > 0,
                                               element(1,T) =:= fe:fst(T))))},

     { "return 'undefined' when applied to an empty tuple",
       ?_assertMatch(undefined, fe:fst({}))}
    ].

snd_test_() ->
    [
     { "be a synonym for erlang:element/2 for pairs and more",
       ?PROP(?FORALL(T, tuple(int()), ?IMPLIES(tuple_size(T) > 1,
                                               element(2,T) =:= fe:snd(T))))},

     { "return 'undefined' when applied to an empty tuple",
       ?_assertMatch(undefined, fe:snd({}))},

     { "return 'undefined' when applied to an singleton tuple",
       ?_assertMatch(undefined, fe:snd({1}))}
    ].

curries_test_() ->
    UncurFun = fun({A, B}) -> A + B end,
    CurFun = fun(A, B) -> A + B end,

    [
     { "fe:curry/1 correctly curries a function",
       ?_assertMatch(2, (fe:curry(UncurFun))(1,1))},

     { "fe:uncurry/1 correctly uncurries a function",
       ?_assertMatch(2, (fe:uncurry(CurFun))({1,1}))},

     { "curry and uncurry are inverse",
       ?_assertMatch(2, (fe:curry(fe:uncurry(CurFun)))(1,1))},

     { "uncurry and curry are inverse",
       ?_assertMatch(2, (fe:uncurry(fe:curry(UncurFun)))({1,1}))}
    ].

%% Collections tests

count_test_() ->
    [
     { "returns the count of needles found in the haystack",
       ?_assertMatch(2, fe:count(2, [1,4,2,4,2])) },

     { "count must never be greater than the length of list",
       ?PROP(?FORALL({X, Xs}, {int(), list(int())},
                     length(Xs) >= fe:count(X, Xs))) },

     { "count must always be >= 0",
       ?PROP(?FORALL({X, Xs}, {int(), list(int())},
                     0 =< fe:count(X, Xs))) }

    ].

uniq_test_() ->
    { "returns the ordered set of uniq items from a list",
        ?_assertMatch([1,2,3], fe:uniq([2,1,1,3,3,2])) }.

foldl1_test_() ->
    Id = fun(X, _Acc) -> X end,
    Mult = fun(N, M) -> N * M end,

    [
     { "It cannot be applied to an empty list.",
       ?_assertError(function_clause, fe:foldl1(Id, []))},

     { "It behaves as foldl, save that Acc is the initial list element.",
       ?_assertMatch(120, fe:foldl1(Mult, [1,2,3,4,5]))}
    ].

find_test_() ->
    [
        { "returns the element in haystack equal to needle",
            ?_assertMatch(needle, fe:find(needle, [thread, needle, spool])) },

        { "returns notfound if the haystack does not contain the needle",
            ?_assertMatch(notfound, fe:find(needle, [thread, spool, cloth])) },

        { "returns a custom value if not found",
            ?_assertMatch(ohai, fe:find(needle, [thread, spool], ohai)) }
    ].

tmap_node_test_() ->
    Addr = fun(_Name, I) -> I + 1 end,
    LeafNode = {name, 2},
    EmptyBranch = {props, {[]}},
    SomeBranch = {props, {[LeafNode, LeafNode]}},
    DeeperBranch = {props, {[EmptyBranch, SomeBranch]}},

    [
     { "it will apply to the value of a leaf",
       ?_assertMatch({name, 3}, fe:tmap_node(Addr, LeafNode))},

     { "it will act as identity over an empty branch",
       ?_assertMatch(EmptyBranch, fe:tmap_node(Addr, EmptyBranch))},

     { "it will recurse down a branch",
       ?_assertMatch({props, {[{name, 3}, {name, 3}]}},
                     fe:tmap_node(Addr, SomeBranch))},

     { "it will recurse down many branches",
       ?_assertMatch({props, {[EmptyBranch, {props, {[{name, 3}, {name,3}]}}]}},
                     fe:tmap_node(Addr, DeeperBranch))}
    ].

tmap_test_() ->
    Addr = fun
               (_Name, I) when is_integer(I) ->
                   I + 1;
               (_Name, Is) when is_list(Is) ->
                   lists:foldl(fun(N, Acc) -> N + Acc end, 0, Is)
           end,

    SimpleTree = {[{name, {[{sub_name, 3}]}}]},
    ComplexTree = {[ {leaf_a, 10},
                    {leaf_b, {[ {leaf_c, 1000},
                               {leaf_d, [1,2,3,4]}
                             ]}},
                    {leaf_e, {[]}}
                  ]},

    [
     { "it will map over a simple tree",
       ?_assertMatch({[{name, {[{sub_name, 4}]}}]}, fe:tmap(Addr, SimpleTree))},

     { "it will map over a complex tree",
       ?_assertMatch({[{leaf_a, 11},
                       {leaf_b, {[ {leaf_c, 1001},
                                   {leaf_d, 10}
                                 ]}},
                       {leaf_e, {[]}}]},
                     fe:tmap(Addr, ComplexTree))}
    ].

%% Utility tests
true_test() -> true = fe:true().
false_test() -> false = fe:false().

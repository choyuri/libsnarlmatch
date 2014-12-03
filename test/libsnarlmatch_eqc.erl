-module(libsnarlmatch_eqc).

-ifdef(TEST).
-ifdef(EQC).
-define(EQC_NUM_TESTS, 500).
-include_lib("fqc/include/fqc.hrl").
-compile(export_all).

test_permission() ->
    not_empty(list(non_blank_string())).

permission() ->
    oneof([
           not_empty(list(oneof([non_blank_string(), <<"_">>]))),
           ?LET(L, list(oneof([non_blank_string(), <<"_">>])),
                L ++ [<<"...">>])
          ]).

tree() ->
    ?SIZED(Size, tree(Size)).

tree(Size) ->
    ?LAZY(
       oneof(
         [ {libsnarlmatch:new(), []} || Size == 0 ]
         ++ [?LETSHRINK(
                [{T, L}], [tree(Size - 1)],
                ?LET(P, permission(), {libsnarlmatch:add(P, T), [P | L]}))
             || Size > 0
            ])).

permissions() ->
    not_empty(list(permission())).

match([], []) ->
    true;

match([], [<<"...">>]) ->
    false;

match(_, [<<"...">>]) ->
    true;

match([], [<<"...">>|_Rest]) ->
    false;

match([], [_X|_R]) ->
    false;

match([X | InRest], [<<"...">>, X|TestRest] = Test) ->
    match(InRest, TestRest) orelse match(InRest, Test);

match([_,X|InRest], [<<"...">>, X|TestRest] = Test) ->
    match(InRest, TestRest) orelse match([X| InRest], Test);

match([_ | InRest], [<<"...">>|_TestRest] = Test) ->
     match(InRest, Test);

match([X|InRest], [X|TestRest]) ->
    match(InRest, TestRest);

match([_|InRest], [<<"_">>|TestRest]) ->
    match(InRest, TestRest);

match(_, _) ->
    false.

test_perms(_Perm, []) ->
    false;

test_perms(Perm, [Test|Tests]) ->
    match(Perm, Test) orelse test_perms(Perm, Tests).

prop_list_convert() ->
    ?FORALL(LIn, permissions(),
            begin
                T1 = libsnarlmatch:from_list(LIn),
                L1 = libsnarlmatch:to_list(T1),
                T2 = libsnarlmatch:from_list(L1),
                L2 = libsnarlmatch:to_list(T1),
                ?WHENFAIL(io:format(
                            user,
                            "LIn : ~p~n"
                            "L1  : ~p~n"
                            "L2  : ~p~n"
                            "T1  : ~p~n"
                            "T2  : ~p~n",
                            [LIn, L1, L2, T1, T2]),
                          T1 == T2 andalso L1 == L2)
            end).

prop_compare_from_list() ->
    ?FORALL({L, P}, {permissions(), test_permission()},
            begin
                T = libsnarlmatch:from_list(L),
                Res1 = test_perms(P, L),
                Res2 = libsnarlmatch:test_perms(P, T),
                ?WHENFAIL(io:format(
                            user,
                            "L : ~p~n"
                            "P : ~p~n"
                            "T : ~p~n"
                            "=> ~p =/= ~p~n",
                            [L, T, T, Res1, Res2]),
                          Res1 == Res2)
            end).

prop_compare_build() ->
    ?FORALL({{T, L}, P}, {tree(), test_permission()},
            begin
                Res1 = test_perms(P, L),
                Res2 = libsnarlmatch:test_perms(P, T),
                ?WHENFAIL(io:format(
                            user,
                            "L : ~p~n"
                            "P : ~p~n"
                            "R : ~p~n"
                            "=> ~p =/= ~p~n",
                            [L, P, T, Res1, Res2]),
                          Res1 == Res2)
            end).

prop_test_all_allowed() ->
    ?FORALL({T, L}, tree(),
            begin
                R1 = [libsnarlmatch:test_perms(P, T) || P <- L],
                R2 = [V || V <- R1, V =/= true],
                ?WHENFAIL(io:format(
                            user,
                            "L : ~p~n"
                            "T : ~p~n"
                            "R1 : ~p~n"
                            "R2 : ~p~n",
                            [L, T, R1, R2]),
                          R2 == [])
            end).

-endif.
-endif.


%% match_direct_test() ->
%%     ?assert(true == match([<<"some_permission">>], [<<"some_permission">>])).

%% nomatch_direct_test() ->
%%     ?assert(false == match([<<"some_permission">>], [<<"some_other_permission">>])).

%% match_direct_list_test() ->
%%     ?assert(true == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>])).

%% nomatch_direct_list_test() ->
%%     ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"other">>, <<"permission">>])),
%%     ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"other_permission">>])).

%% nomatch_short_list_test() ->
%%     ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>])).

%% nomatch_long_list_test() ->
%%     ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>, yap])).

%% match_tripoint_test() ->
%%     ?assert(true == match([<<"some">>, <<"permission">>], [<<"...">>])).

%% match_tripoint_at_end_test() ->
%%     ?assert(true == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>, <<"...">>])).

%% match_tripoint_start_test() ->
%%     ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"...">>, <<"permission">>])).

%% match_tripoint_end_test() ->
%%     ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"...">>])).

%% match_tripoint_middle_test() ->
%%     ?assert(true == match([<<"some">>, <<"really">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"...">>, <<"permission">>])).

%% match_underscore_test() ->
%%     ?assert(true == match([some], [<<"_">>])).

%% match_underscore_start_test() ->
%%     ?assert(true == match([<<"some">>, <<"permission">>], [<<"_">>, <<"permission">>])).

%% match_underscore_end_test() ->
%%     ?assert(true == match([<<"some">>, <<"permission">>], [<<"some">>, <<"_">>])).

%% match_underscore_middle_test() ->
%%     ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"_">>, <<"permission">>])).

%% nomatch_underscore_double_test() ->
%%     ?assert(false == match([<<"some">>, <<"really">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"_">>, <<"permission">>])).


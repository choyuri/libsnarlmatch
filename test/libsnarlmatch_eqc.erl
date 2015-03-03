-module(libsnarlmatch_eqc).

-ifdef(TEST).
-ifdef(EQC).
-define(EQC_NUM_TESTS, 500).
-include_lib("fqc/include/fqc.hrl").
-compile(export_all).

-define(L, libsnarlmatch).
-define(T, libsnarlmatch_tree).

test_permission() ->
    not_empty(list(non_blank_string())).

permission_prefix() ->
    list(frequency([{9,non_blank_string()}, {1, <<"_">>}])).

permission() ->
    frequency([
               {20, not_empty(permission_prefix())},
               {1, ?LET(L, permission_prefix(), L ++ [<<"...">>])}
              ]).


no_admin_permission() ->
    ?SUCHTHAT(P, permission(),
              [E || E  <- P, E =/= <<"_">>, E =/= <<"...">>] =/= []).

tree() ->
    ?SIZED(Size, tree(Size)).

tree(Size) ->
    ?LAZY(
       oneof(
         [ {?T:new(), ?L:new()} || Size == 0 ]
         ++ [?LETSHRINK(
                [{T, L}], [tree(Size - 1)],
                ?LET(P, permission(), {?L:add(P, T), ?L:add(P, L)}))
             || Size > 0
            ])).


no_admin_tree() ->
    ?SIZED(Size, no_admin_tree(Size)).

no_admin_tree(Size) ->
    ?LAZY(
       oneof(
         [ {?T:new(), ?L:new()} || Size == 0 ]
         ++ [?LETSHRINK(
                [{T, L}], [no_admin_tree(Size - 1)],
                ?LET(P, no_admin_permission(), {?L:add(P, T), ?L:add(P, L)}))
             || Size > 0
            ])).

tree_and_bad_perm() ->
    ?LET({T, L}, no_admin_tree(),
         ?LET(P,
              ?SUCHTHAT(P, test_permission(),
                        not ?L:test_perms(P, L)), {T, P})).

permissions() ->
    not_empty(list(permission())).

prop_no_match() ->
    ?FORALL({T, P}, tree_and_bad_perm(),
            ?WHENFAIL(io:format(
                        user,
                        "T : ~p~n"
                        "P : ~p~n",
                        [T, P]),
                      not ?L:test_perms(P, T))).

prop_list_convert() ->
    ?FORALL(LIn, permissions(),
            begin
                T1 = ?T:from_list(LIn),
                L1 = ?T:to_list(T1),
                T2 = ?T:from_list(L1),
                L2 = ?T:to_list(T1),
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
                T = ?T:from_list(L),
                Res1 = ?L:test_perms(P, L),
                Res2 = ?L:test_perms(P, T),
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
                Res1 = ?L:test_perms(P, L),
                Res2 = ?T:test_perms(P, T),
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
                R1 = [?T:test_perms(P, T) || P <- L],
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

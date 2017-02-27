-module(libsnarlmatch).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([test_perms/2, match/2, new/0, add/2, to_list/1]).

-type permission_check() :: permission() |
                            {'or', permission(), permission()} |
                            {'and', permission(), permission()}.

-type permission() :: [binary()].
-type permissions() :: [permission()] | libsnarlmatch_tree:tree().

new() ->
    [].

-spec test_perms(Permissions::permission_check(), Permissions::permissions()) ->
                   true | false.

test_perms({'or', PLeft, PRight}, Permissions) ->
    test_perms(PLeft, Permissions) orelse test_perms(PRight, Permissions);

test_perms({'and', PLeft, PRight}, Permissions) ->
    test_perms(PLeft, Permissions) andalso test_perms(PRight, Permissions);

test_perms(Perm, {tree, _} = Tree) ->
    libsnarlmatch_tree:test_perms(Perm, Tree);

test_perms(_Perm, []) ->
    false;

test_perms(Perm, [Test|Permissions]) ->
    match(Perm, Test) orelse test_perms(Perm, Permissions).

add(Perm, {tree,_} = Tree) ->
    libsnarlmatch_tree:add(Perm, Tree);

add(Perm, Perms) ->
    [Perm | Perms].

to_list({tree, _} = Tree) ->
    libsnarlmatch_tree:to_list(Tree);

to_list(Perms) ->
    Perms.

-spec match(Permission::permission(), Permissions::permission()) ->
                   true | false.
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



-ifdef(TEST).


match_direct_test() ->
    ?assert(true == match([<<"some_permission">>], [<<"some_permission">>])).

nomatch_direct_test() ->
    ?assert(false == match([<<"some_permission">>], [<<"some_other_permission">>])).

match_direct_list_test() ->
    ?assert(true == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>])).

nomatch_direct_list_test() ->
    ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"other">>, <<"permission">>])),
    ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"other_permission">>])).

nomatch_short_list_test() ->
    ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>])).

nomatch_long_list_test() ->
    ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>, yap])).

match_tripoint_test() ->
    ?assert(true == match([<<"some">>, <<"permission">>], [<<"...">>])).

match_tripoint_at_end_test() ->
    ?assert(false == match([<<"some">>, <<"permission">>], [<<"some">>, <<"permission">>, <<"...">>])).

match_tripoint_start_test() ->
    ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"...">>, <<"permission">>])).

match_tripoint_end_test() ->
    ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"...">>])).

match_tripoint_middle_test() ->
    ?assert(true == match([<<"some">>, <<"really">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"...">>, <<"permission">>])).

match_underscore_test() ->
    ?assert(true == match([some], [<<"_">>])).

match_underscore_start_test() ->
    ?assert(true == match([<<"some">>, <<"permission">>], [<<"_">>, <<"permission">>])).

match_underscore_end_test() ->
    ?assert(true == match([<<"some">>, <<"permission">>], [<<"some">>, <<"_">>])).

match_underscore_middle_test() ->
    ?assert(true == match([<<"some">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"_">>, <<"permission">>])).

nomatch_underscore_double_test() ->
    ?assert(false == match([<<"some">>, <<"really">>, <<"cool">>, <<"permission">>], [<<"some">>, <<"_">>, <<"permission">>])).

-endif.

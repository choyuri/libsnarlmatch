-module(libsnarlmatch).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([test_perms/2, match/2, new/0]).

-type permission() :: [binary()].
-type permission_matcher() :: [binary()].


new() ->
    [].

-spec match(Permission::permission(), Matcher::permission_matcher()) ->
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

-spec test_perms(Permissions::[permission()], Matcher::permission_matcher()) ->
                   true | false.
test_perms(_Perm, []) ->
    false;

test_perms(Perm, [Test|Tests]) ->
    match(Perm, Test) orelse test_perms(Perm, Tests).

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

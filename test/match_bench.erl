-module(match_bench).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(SIZE, 50000).

entry(false) ->
    <<($a + random:uniform($z - $a))>>;

entry(true) ->
    case random:uniform(20) of
        1 ->
            <<"_">>;
        _ ->
            entry(false)
    end.

permission(0, false) ->
    [];

permission(0, true) ->
    case random:uniform(100) of
        1 ->
            [<<"...">>];
        _ ->
            permission(0, false)
    end;

permission(N, W) ->
    [entry(W) | permission(N-1, W)].

permissions() ->
    permissions(?SIZE).

permissions(0) ->
    [];
permissions(N) ->
    [permission(random:uniform(4), true) | permissions(N-1)].

match([], []) ->
    true;

match([], [<<"...">>]) ->
    false;

match(_, [<<"...">>]) ->
    true;

match([], [_X|_R]) ->
    false;

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

bench_test() ->
    L = permissions(),
    P = permission(3, false),
    T = libsnarlmatch:from_list(L),
    ?debugTime("List: ", test_perms(P, L)),
    ?debugTime("Tree: ", libsnarlmatch:test_perms(P, T)),
    true.

-endif.

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

test_perms(Perm, [Test|Tests]) ->
    libsnarlmatch:match(Perm, Test) orelse libsnarlmatch:test_perms(Perm, Tests).

bench_dict_test_() ->
    L = permissions(),
    P = permission(3, false),
    T = libsnarlmatch_tree:from_list(L),
    [
     {timeout, 60, ?_assert(?debugTime("List:     ", bench_list(P, L)))},
     {timeout, 60, ?_assert(?debugTime("Conv Tree ", bench_conv_tree(L)))},
     {timeout, 60, ?_assert(?debugTime("Tree      ", bench_tree(P, T)))}
    ].



bench_conv_tree(L) ->
    [begin
         libsnarlmatch_tree:from_list(L)
     end || _ <- lists:seq(0, 100)], true.

bench_list(P, L) ->
    [begin
         libsnarlmatch:test_perms(P, L)
     end || _ <- lists:seq(0,10000)], true.

bench_tree(P, T) ->
    [begin
         libsnarlmatch_tree:test_perms(P, T)
     end || _ <- lists:seq(0,10000)], true.

-endif.

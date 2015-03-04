-module(match_bench).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(SIZE, 1000).

entry(false) ->
    <<($a + random:uniform($z - $a))>>;

entry(true) ->
    case random:uniform(50) of
        1 ->
            <<"_">>;
        _ ->
            entry(false)
    end.

permission(0) ->
    [];

permission(N) ->
    [entry(true) | permission(N-1)].

permissions() ->
    permissions(?SIZE).

permissions(0) ->
    [];
permissions(N) ->
    [permission(random:uniform(3) + 1) | permissions(N-1)].

test_perms(Perm, [Test|Tests]) ->
    libsnarlmatch:match(Perm, Test) orelse libsnarlmatch:test_perms(Perm, Tests).

bench_dict_test_() ->
    L = lists:usort(permissions()),
    P = permission(3),
    T = libsnarlmatch_tree:from_list(L),
    [
     {timeout, 120, ?_assert(?debugTime("List:     ", bench_list(P, L)))},
     {timeout, 120, ?_assert(?debugTime("Conv Tree ", bench_conv_tree(L)))},
     {timeout, 120, ?_assert(?debugTime("Conv List ", bench_conv_list(T)))},
     {timeout, 120, ?_assert(?debugTime("Tree      ", bench_tree(P, T)))}
    ].



bench_conv_tree(L) ->
    %%fprof:trace(start, "from_list.trace"),
    [begin
         libsnarlmatch_tree:from_list(L)
     end || _ <- lists:seq(0, 10000)],
    %%fprof:trace(stop),
    true.

bench_conv_list(T) ->
    %%fprof:trace(start, "to_list.trace"),
    [begin
         libsnarlmatch_tree:to_list(T)
     end || _ <- lists:seq(0, 10000)],
    %%fprof:trace(stop),
    true.


bench_list(P, L) ->
    [begin
         libsnarlmatch:test_perms(P, L)
     end || _ <- lists:seq(0,10000)], true.

bench_tree(P, T) ->
    [begin
         libsnarlmatch_tree:test_perms(P, T)
     end || _ <- lists:seq(0,10000)], true.

-endif.

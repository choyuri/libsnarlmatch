-module(libsnarlmatch_tree).

-export([test_perms/2, minify/1, new/0, add/2, from_list/1, to_list/1,
         merge/2]).

-export_type([tree/0]).

-type tree() :: {tree, maps:map()}.

%%-type permission() :: [binary()].
%%-type permission_matcher() :: [binary()].

-define(O, maps).
-define(END, #{nothing => true}).
-define(EMPTY, #{}).
-define(SPLIT_LEN, 1000).

new() ->
    {tree, ?EMPTY}.


add(Perm, {tree, Dict}) ->
    {tree, add_tree(Perm, Dict)}.

test_perms(Perm, {tree, Dict}) ->
    test_perms_tree(Perm, Dict).

%% from_list(L) when length(L) > ?SPLIT_LEN ->
%%     {H, T} = lists:split(?SPLIT_LEN, L),
%%     merge(from_list(H), from_list(T));

from_list(L) ->
    {tree, lists:foldl(fun add1/2, ?EMPTY, L)}.

to_list({tree, P}) ->
    fold_tree(fun(E, Acc) -> [E | Acc] end, [], P).

merge({tree, A}, {tree, B}) ->
    {tree, fold_tree(fun add_tree/2, B, A)}.

minify({tree, A}) ->
    {tree, fold_tree(fun add_tree/2, ?EMPTY, A)}.
%%from_list(to_list(A)).

add_tree(Perm, Dict) ->
    case lists:member(<<"...">>, Perm) of
        true ->
            add1(Perm, Dict);
        _ ->
            case test_perms_tree(Perm, Dict) of
                true ->
                    Dict;
                _ ->
                    add1(Perm, Dict)
            end
    end.

add1([], Dict) ->
    Dict#{nothing => true};

add1(_, Dict = #{'...' := _}) ->
    Dict;


add1([<<"...">>], #{nothing := true}) ->
    #{'...' => ?END, nothing => true};

add1([<<"...">>], _) ->
    #{'...' => ?END};

add1([<<"_">> | T], Dict) ->
    add1(['_' | T], Dict);

add1(_, Dict = #{'...' := _}) ->
    Dict;

add1([E | T], Dict = #{'_' := Sub}) ->
    case test_perms_tree(T, Sub) of
        true ->
            Dict;
        false ->
            V1 = case ?O:find(E, Dict) of
                     {ok, V} ->
                         add1(T, V);
                     _ ->
                         addq(T)
                 end,
            Dict#{E => V1}
    end;

add1([E | T], Dict) ->
    V1 = case ?O:find(E, Dict) of
             {ok, V} ->
                 add1(T, V);
             _ ->
                 addq(T)
         end,
    Dict#{E => V1}.

addq([]) ->
    ?END;

addq([<<"...">>]) ->
    #{'...' => ?END};

addq([<<"_">> | T]) ->
    #{'_' => addq(T)};

addq([E | T]) ->
    #{E => addq(T)}.

test_perms_tree([], #{nothing := true}) ->
    true;

test_perms_tree([], _) ->
    false;

test_perms_tree([_ | _], #{'...' := _}) ->
    true;

%% If we see the '_' we need to test if its children match
%% but if not it still could be in the others.
test_perms_tree([E | T], #{'_' := Sub} = Full) ->
    test_perms_tree(T, Sub) orelse
        test_perms_tree([E | T], maps:remove('_', Full));

test_perms_tree([E | T], Dict) ->
    case ?O:find(E, Dict) of
        {ok, Sub} ->
            test_perms_tree(T, Sub);
        _ ->
            false
    end.

fold_tree(Fun, Acc0, P) ->
    fold([], Fun, Acc0, P).

fold(Pfx, Fun, Acc0, P) ->
    lists:foldl(fun({nothing, true}, Acc) ->
                        Fun(lists:reverse(Pfx), Acc);
                   ({'_', Ps}, Acc) ->
                        fold([<<"_">> | Pfx], Fun, Acc, Ps);
                   ({'...', Ps}, Acc) ->
                        fold([<<"...">> | Pfx], Fun, Acc, Ps);
                   ({E, Ps}, Acc) ->
                        fold([E | Pfx], Fun, Acc, Ps)
                end, Acc0, lists:sort(?O:to_list(P))).

-module(libsnarlmatch_tree).

-export([test_perms/2, new/0, add/2, from_list/1, to_list/1]).

%-type permission() :: [binary()].
%-type permission_matcher() :: [binary()].
-define(O, orddict).

new() ->
    {tree, ?O:new()}.


add(Perm, {tree, Dict}) ->
    {tree, add_tree(Perm, Dict)}.

test_perms(Perm, {tree, Dict}) ->
    test_perms_tree(Perm, Dict).

from_list(L) ->
    {tree, lists:foldl(fun add_tree/2, [], lists:usort(L))}.

to_list({tree, P}) ->
    fold_tree(fun(E, Acc) -> [E | Acc] end, [], P).

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
    ?O:store(nothing, true, Dict);

add1([<<"...">>], Dict) ->
    case ?O:find(nothing, Dict) of
        {ok, true} ->
            [{'...',[{nothing,true}]},{nothing,true}];
        _ ->
            [{'...', [{nothing, true}]}]
    end;

add1([<<"_">> | T], Dict) ->
    add1(['_' | T], Dict);

add1([E | T], []) ->
    [{E, addq(T)}];

add1([E | T], Dict) ->
    V1 = case ?O:find(E, Dict) of
            {ok, V} ->
                add1(T, V);
            _ ->
                addq(T)
        end,
    ?O:store(E, V1, Dict).

addq([]) ->
    [{nothing, true}];

addq([<<"...">>]) ->
    [{'...', [{nothing, true}]}];

addq([<<"_">> | T]) ->
    [{'_', addq(T)}];
addq([E | T]) ->
    [{E, addq(T)}].


test_perms_tree([], Dict) ->
    {ok, true} == ?O:find(nothing, Dict);

test_perms_tree(_Perm, []) ->
    false;

test_perms_tree([_ | _], [{'...', _} | _ ])->
    true;

test_perms_tree([E | T], [{'_', Dict} | Rest ])->
    case test_perms_tree(T, Dict) of
        true ->
            true;
        false ->
            test_perms_tree([E | T], Rest)
    end;

test_perms_tree([E | T], Dict) ->
    case ?O:find(E, Dict) of
        {ok, V2} ->
            test_perms_tree(T, V2);
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
                end, Acc0, P).

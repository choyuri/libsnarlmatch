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
    {tree, lists:foldl(fun add_tree/2, ?O:new(), lists:usort(L))}.

to_list({tree, P}) ->
    to_list_tree(P).

add_tree(Perm, Dict) ->
    case {lists:member(<<"...">>, Perm), test_perms_tree(Perm, Dict)} of
        {false, true} ->
            Dict;
        _ ->
            add1(Perm, Dict)
    end.

add1([], Dict) ->
    ?O:store(nothing, true, Dict);

add1([<<"...">>], Dict) ->
    D1 = ?O:store('...', ?O:store(nothing, true, ?O:new()), ?O:new()),
    case ?O:find(nothing, Dict) of
        {ok, true} ->
            ?O:store(nothing, true, D1);
        _ ->
            D1
    end;

add1([<<"_">> | T], Dict) ->
    add1(['_' | T], Dict);

add1([E | T], Dict) ->
    V1 = case ?O:find(E, Dict) of
             {ok, V} ->
                 V;
             _ ->
                 ?O:new()
         end,
    V2 = add_tree(T, V1),
    ?O:store(E, V2, Dict).

test_perms_tree([], Dict) ->
    {ok, true} == ?O:find(nothing, Dict);

test_perms_tree(_Perm, []) ->
    false;

test_perms_tree([E | T], Dict) ->
    case ?O:find('...', Dict) of
        {ok, _} ->
            true;
        _ ->
            R1 = case ?O:find('_', Dict) of
                     {ok, V1} ->
                         test_perms_tree(T, V1);
                     _ ->
                         false
                 end,
            case {R1, ?O:find(E, Dict)} of
                {true, _} ->
                    true;
                {_, {ok, V2}} ->
                    test_perms_tree(T, V2);
                _ ->
                    false
            end
    end.

to_list_tree(P)->
    lists:foldl(fun(E, Acc) ->
                        E1 = to_list1(E),
                        Acc1 = Acc ++ E1,
                        Acc1
                end, [], P).


to_list1({nothing, true}) ->
    [[]];

to_list1({'_', Ps}) ->
    [ [<<"_">> | P] || P <- to_list_tree(Ps)];

to_list1({'...', Ps}) ->
    [ [<<"...">> | P] || P <- to_list_tree(Ps)];

to_list1({K, Ps}) ->
    [ [K | P] || P <- to_list_tree(Ps)].

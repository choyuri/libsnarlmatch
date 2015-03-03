-module(libsnarlmatch_tree).

-export([test_perms/2, new/0, add/2, from_list/1, to_list/1]).

%-type permission() :: [binary()].
%-type permission_matcher() :: [binary()].

new() ->
    orddict:new().

add(Perm, Dict) ->
    case {lists:member(<<"...">>, Perm), test_perms(Perm, Dict)} of
        {false, true} ->
            Dict;
        _ ->
            add1(Perm, Dict)
    end.

add1([], Dict) ->
    orddict:store(nothing, true, Dict);

add1([<<"...">>], Dict) ->
    D1 = orddict:store('...', orddict:store(nothing, true, new()), new()),
    case orddict:find(nothing, Dict) of
        {ok, true} ->
            orddict:store(nothing, true, D1);
        _ ->
            D1
    end;

add1([<<"_">> | T], Dict) ->
    add1(['_' | T], Dict);

add1([E | T], Dict) ->
    V1 = case orddict:find(E, Dict) of
             {ok, V} ->
                 V;
             _ ->
                 new()
         end,
    V2 = add(T, V1),
    orddict:store(E, V2, Dict).

test_perms([], Dict) ->
    {ok, true} == orddict:find(nothing, Dict);

test_perms(_Perm, []) ->
    false;

test_perms([E | T], Dict) ->
    case orddict:find('...', Dict) of
        {ok, _} ->
            true;
        _ ->
            R1 = case orddict:find('_', Dict) of
                     {ok, V1} ->
                         test_perms(T, V1);
                     _ ->
                         false
                 end,
            case {R1, orddict:find(E, Dict)} of
                {true, _} ->
                    true;
                {_, {ok, V2}} ->
                    test_perms(T, V2);
                _ ->
                    false
            end
    end.

from_list(L) ->
    lists:foldl(fun add/2, new(), lists:usort(L)).

to_list(P) ->
    lists:foldl(fun(E, Acc) ->
                        E1 = to_list1(E),
                        Acc1 = Acc ++ E1,
                        Acc1
                end, [], P).


to_list1({nothing, true}) ->
    [[]];

to_list1({'_', Ps}) ->
    [ [<<"_">> | P] || P <- to_list(Ps)];

to_list1({'...', Ps}) ->
    [ [<<"...">> | P] || P <- to_list(Ps)];

to_list1({K, Ps}) ->
    [ [K | P] || P <- to_list(Ps)].

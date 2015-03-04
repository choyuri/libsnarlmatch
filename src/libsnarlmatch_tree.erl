-module(libsnarlmatch_tree).

-export([test_perms/2, minify/1, new/0, add/2, from_list/1, to_list/1,
         merge/2]).

%%-type permission() :: [binary()].
%%-type permission_matcher() :: [binary()].

-define(O, orddict).
-define(END, ?O:store(nothing, true, ?O:new())).
-define(EMPTY, ?O:new()).
-define(SPLIT_LEN, 1000).

new() ->
    {tree, ?O:new()}.


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

add1(_, [{'...', _} | _] = Dict) ->
    Dict;

add1([<<"...">>], Dict) ->
    D = ?O:store('...', ?END, ?EMPTY),
    case ?O:find(nothing, Dict) of
        {ok, true} ->
            ?O:store(nothing, true, D);
        _ ->
            D
    end;

add1([<<"_">> | T], Dict) ->
    add1(['_' | T], Dict);

add1([E | T], Dict) ->
    case ?O:find('...', Dict) of
        {ok, _} ->
            Dict;
        _ ->
            V1 = case ?O:find(E, Dict) of
                     {ok, V} ->
                         add1(T, V);
                     _ ->
                         addq(T)
                 end,
            ?O:store(E, V1, Dict)
    end.

addq([]) ->
    ?END;

addq([<<"...">>]) ->
    ?O:store('...', ?END, ?EMPTY);

addq([<<"_">> | T]) ->
    ?O:store('_', addq(T), ?EMPTY);

addq([E | T]) ->
    ?O:store(E, addq(T), ?EMPTY).

test_perms_tree([], Dict) ->
    {ok, true} == ?O:find(nothing, Dict);

test_perms_tree(_Perm, []) ->
   false;

test_perms_tree([_ | _], [{'...', _} | _ ])->
    true;

%% If we see the '_' we need to test if its children match
%% but if not it still could be in the others.
test_perms_tree([E | T], [{'_', Dict} | Rest ])->
    test_perms_tree(T, Dict) orelse
        test_perms_tree([E | T], Rest);

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
    ?O:fold(fun(nothing, true, Acc) ->
                    Fun(lists:reverse(Pfx), Acc);
               ('_', Ps, Acc) ->
                    fold([<<"_">> | Pfx], Fun, Acc, Ps);
               ('...', Ps, Acc) ->
                    fold([<<"...">> | Pfx], Fun, Acc, Ps);
               (E, Ps, Acc) ->
                    fold([E | Pfx], Fun, Acc, Ps)
            end, Acc0, P).

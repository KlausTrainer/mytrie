-module(mytrie).

-export([new/0, store/3, find/2, find_longest_prefix/2]).

-type trie() :: dict().
-export_type([trie/0]).

-spec new() -> trie().
new() ->
    dict:new().

-spec store(nonempty_string(), any(), trie()) -> trie().
store([H|T], Value, Trie) ->
    NewEntry = case dict:find(H, Trie) of
    error when T =:= []->
        {Trie, Value};
    error ->
        {store(T, Value, Trie), nil};
    {ok, {Trie2, _Value2}} when T =:= []->
        {Trie2, Value};
    {ok, {Trie2, Value2}} ->
        {store(T, Value, Trie2), Value2}
    end,
    dict:store(H, NewEntry, Trie).

-spec find(nonempty_string(), trie()) -> error | {ok, any()}.
find([H|T], Trie) ->
    case dict:find(H, Trie) of
    error ->
        error;
    {ok, {_SubTrie, Value}} when T =:= [] ->
        case Value of
        nil ->
            error;
        _ ->
            {ok, Value}
        end;
    {ok, {SubTrie, _Value}} ->
        find(T, SubTrie)
    end.

-spec find_longest_prefix(nonempty_string(), trie()) -> error | {ok, pos_integer(), any()}.
find_longest_prefix(Key, Trie) ->
    find_longest_prefix(Key, Trie, 0, 0, nil).

-spec find_longest_prefix(nonempty_string(), trie(), non_neg_integer(), non_neg_integer(), any()) -> error | {ok, pos_integer(), any()}.
find_longest_prefix([H|T], Trie, I, PrefixLength, PrefixValue) ->
    case dict:find(H, Trie) of
    error when PrefixValue =:= nil ->
        error;
    error ->
        {ok, PrefixLength, PrefixValue};
    {ok, {_SubTrie, Value}} when T =:= [] ->
        case Value of
        nil when PrefixValue =:= nil ->
            error;
        nil ->
            {ok, PrefixLength, PrefixValue};
        _ ->
            {ok, I + 1, Value}
        end;
    {ok, {SubTrie, Value}} ->
        case Value of
        nil ->
            find_longest_prefix(T, SubTrie, I + 1, PrefixLength, PrefixValue);
        _ ->
            find_longest_prefix(T, SubTrie, I + 1, I + 1, Value)
        end
    end.

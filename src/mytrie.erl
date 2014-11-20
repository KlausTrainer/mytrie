-module(mytrie).

-export([new/0, store/3, find/2, find_longest_prefix/2]).

-type entry() :: {trie(), binary() | []} | binary().
-type trie() :: list({char(), entry()}) | dict:dict(char(), entry()).
-export_type([trie/0]).

-define(MAX_LIST_SIZE, 8).

-spec new() -> trie().
new() ->
    [].

-spec store(nonempty_string(), binary(), trie()) -> trie().
store([H|T], NewValue, Trie) when is_binary(NewValue) ->
    NewEntry = case find_entry(H, Trie) of
    error when T =:= [] ->
        NewValue;
    error ->
        {store(T, NewValue, Trie), []};
    {ok, {SubTrie, _OldValue}} when T =:= [] ->
        {SubTrie, NewValue};
    {ok, _OldValue} when T =:= [] ->
        NewValue;
    {ok, {SubTrie, Value}} ->
        {store(T, NewValue, SubTrie), Value};
    {ok, Value} ->
        {store(T, NewValue, []), Value}
    end,
    case is_list(Trie) of
    true when length(Trie) =< ?MAX_LIST_SIZE ->
        lists:keysort(1, lists:keystore(H, 1, Trie, {H, NewEntry}));
    true ->
        dict:store(H, NewEntry, dict:from_list(Trie));
    _ ->
        dict:store(H, NewEntry, Trie)
    end.

-spec find(nonempty_string(), trie()) -> error | {ok, binary()}.
find([H|T], Trie) ->
    case find_entry(H, Trie) of
    {ok, {_SubTrie, []}} when T =:= [] ->
        error;
    {ok, {_SubTrie, Value}} when T =:= [] ->
        {ok, Value};
    {ok, Value} when T =:= [] ->
        {ok, Value};
    {ok, {SubTrie, _Value}} ->
        find(T, SubTrie);
    _ ->
        error
    end.

-spec find_longest_prefix(nonempty_string(), trie()) -> error | {ok, pos_integer(), binary()}.
find_longest_prefix(Key, Trie) ->
    find_longest_prefix(Key, Trie, 0, 0, []).

-spec find_longest_prefix(nonempty_string(), trie(), non_neg_integer(), non_neg_integer(), any()) -> error | {ok, pos_integer(), any()}.
find_longest_prefix([H|T], Trie, I, PrefixLength, PrefixValue) ->
    case find_entry(H, Trie) of
    error when PrefixValue =/= [] ->
        {ok, PrefixLength, PrefixValue};
    error ->
        error;
    {ok, {_SubTrie, []}} when T =:= [] andalso PrefixValue =:= [] ->
        error;
    {ok, {_SubTrie, []}} when T =:= [] ->
        {ok, PrefixLength, PrefixValue};
    {ok, {_SubTrie, Value}} when T =:= [] ->
        {ok, I + 1, Value};
    {ok, {SubTrie, []}} ->
        find_longest_prefix(T, SubTrie, I + 1, PrefixLength, PrefixValue);
    {ok, {SubTrie, Value}} ->
        find_longest_prefix(T, SubTrie, I + 1, I + 1, Value);
    {ok, Value} ->
        {ok, I + 1, Value}
    end.

-spec find_entry(char(), trie()) -> error | {ok, entry()}.
find_entry(Char, Trie) when is_list(Trie) ->
    find_entry_in_sorted_list(Char, Trie);
find_entry(Char, Trie) ->
    dict:find(Char, Trie).

-spec find_entry_in_sorted_list(char(), list({char(), entry()})) -> error | {ok, entry()}.
find_entry_in_sorted_list(Char, [{Key, _}|T]) when Char < Key ->
    find_entry_in_sorted_list(Char, T);
find_entry_in_sorted_list(Char, [{Key, Entry}|_]) when Char =:= Key ->
    {ok, Entry};
find_entry_in_sorted_list(_, _) ->
    error.

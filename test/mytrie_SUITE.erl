-module(mytrie_SUITE).

-export([all/0, groups/0]).
-export([
    create/1,
    update/1
]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, mytrie}].

groups() ->
    [{mytrie, [], [create, update]}].

create(_) ->
    T = mytrie:new(),
    error = mytrie:find("foo", T),
    T1 = mytrie:store("foo", <<"bar">>, T),
    error = mytrie:find("f", T1),
    error = mytrie:find_longest_prefix("f", T1),
    error = mytrie:find("fo", T1),
    error = mytrie:find_longest_prefix("fo", T1),
    {ok, <<"bar">>} = mytrie:find("foo", T1),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foo", T1),
    error = mytrie:find("foob", T1),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foob", T1),
    T2 = mytrie:store("foobar", <<"baz">>, T1),
    {ok, <<"bar">>} = mytrie:find("foo", T2),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foo", T2),
    error = mytrie:find("foob", T2),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foob", T2),
    error = mytrie:find("fooba", T2),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("fooba", T2),
    {ok, <<"baz">>} = mytrie:find("foobar", T2),
    {ok, 6, <<"baz">>} = mytrie:find_longest_prefix("foobar", T2),
    error = mytrie:find("foobaro", T2),
    {ok, 6, <<"baz">>} = mytrie:find_longest_prefix("foobar", T2).

update(_) ->
    T = mytrie:new(),
    error = mytrie:find("foo", T),
    T1 = mytrie:store("foo", <<"bar">>, T),
    error = mytrie:find("f", T1),
    error = mytrie:find_longest_prefix("f", T1),
    error = mytrie:find("fo", T1),
    error = mytrie:find_longest_prefix("fo", T1),
    {ok, <<"bar">>} = mytrie:find("foo", T1),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foo", T1),
    error = mytrie:find("foob", T1),
    {ok, 3, <<"bar">>} = mytrie:find_longest_prefix("foob", T1),
    T2 = mytrie:store("foo", <<"baz">>, T),
    {ok, <<"baz">>} = mytrie:find("foo", T2),
    {ok, 3, <<"baz">>} = mytrie:find_longest_prefix("foo", T2),
    error = mytrie:find("foob", T2),
    {ok, 3, <<"baz">>} = mytrie:find_longest_prefix("foob", T2),
    T3 = mytrie:store("foobar", <<"pow">>, T2),
    {ok, <<"baz">>} = mytrie:find("foo", T3),
    {ok, 3, <<"baz">>} = mytrie:find_longest_prefix("foo", T3),
    error = mytrie:find("foob", T3),
    {ok, 3, <<"baz">>} = mytrie:find_longest_prefix("foob", T3),
    error = mytrie:find("fooba", T3),
    {ok, 3, <<"baz">>} = mytrie:find_longest_prefix("fooba", T3),
    {ok, <<"pow">>} = mytrie:find("foobar", T3),
    {ok, 6, <<"pow">>} = mytrie:find_longest_prefix("foobar", T3),
    error = mytrie:find("foobaro", T3),
    {ok, 6, <<"pow">>} = mytrie:find_longest_prefix("foobaro", T3).

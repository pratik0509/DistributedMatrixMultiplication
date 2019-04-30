% Module for vector related operations

-module(vector).
-export([dot/2, multiply/2, divide/2, sum/2, difference/2, loc/2, read/1, readvec/0]).

loc(Vector, Idx) ->
    lists:nth(Idx, Vector).

dot([], []) ->
    0;
dot([A0 | ARest], [B0 | BRest]) ->
    A0 * B0 + dot(ARest, BRest).

multiply([], []) ->
    [];
multiply([A0 | ARest], [B0 | BRest]) ->
    [(A0 * B0) | multiply(ARest, BRest)].

divide([], []) ->
    [];
divide([A0 | ARest], [B0 | BRest]) ->
    [(A0 / B0) | divide(ARest, BRest)].

sum([], []) ->
    [];
sum([A0 | ARest], [B0 | BRest]) ->
    [(A0 + B0) | sum(ARest, BRest)].

difference([], []) ->
    [];
difference([A0 | ARest], [B0 | BRest]) ->
    [(A0 - B0) | difference(ARest, BRest)].

read(0) ->
    [];
read(Dim) ->
    {ok, A} = io:read(""),
    [A | read(Dim - 1)].

readvec() ->
    {ok, A} = io:read(""),
    A.

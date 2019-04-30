-module(matrix).
-export([multiply/2, divide/2, difference/2, sum/2,
        transpose/1, dot/2, read/2, readmat/1, row/2,
        col/2, new/2, get/3, set/4]).
-import(vector, []).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

row(Matrix, Idx) ->
    lists:nth(Idx, Matrix).

col(Matrix, Idx) ->
    Matrixt = transpose(Matrix),
    lists:nth(Idx, Matrixt).

dot(A, B) ->
    Bt = transpose(B),
    [[vector:dot(X, Y) || Y <- Bt] || X <- A].

multiply([], []) ->
    [];
multiply([A1 | ARest], [B1 | BRest]) ->
    [vector:multiply(A1, B1) | multiply(ARest, BRest)].

divide([], []) ->
    [];
divide([A1 | ARest], [B1 | BRest]) ->
    [vector:divide(A1, B1) | divide(ARest, BRest)].

difference([], []) ->
    [];
difference([A1 | ARest], [B1 | BRest]) ->
    [vector:difference(A1, B1) | difference(ARest, BRest)].

sum([], []) ->
    [];
sum([A1 | ARest], [B1 | BRest]) ->
    [vector:sum(A1, B1) | sum(ARest, BRest)].

read(0, _) ->
    [];
read(M, N) ->
    [vector:read(N) | read(M - 1, N)].

readmat(0) ->
    [];
readmat(Rows) ->
    [vector:readvec() | readmat(Rows - 1)].

new(Rows, Cols)->
    A = array:new(Rows),
    array:map(fun(_X, _T) -> array:new(Cols) end, A).

get(RowI, ColI, A) ->
    Row = array:get(RowI, A),
    array:get(ColI, Row).

set(RowI, ColI, Ele, A) ->
    Row = array:get(RowI, A),
    Row2 = array:set(ColI, Ele, Row),
    array:set(RowI, Row2, A).
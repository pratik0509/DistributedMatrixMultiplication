-module(helper).
-export([calculate_pairs/2, push_data/4]).
-import(blockstripe, []).

calculate_pairs(A, B) ->
    Alist = lists:seq(1, A),
    Blist = lists:seq(1, B),
    [{X, Y} || X <- Alist, Y <- Blist].

push_data(blockstripe, L, M, N) ->
    A = [[rand:uniform(21)/7 || _ <- lists:seq(1, M)] || _ <- lists:seq(1, L)],
    B = [[rand:uniform(21)/7 || _ <- lists:seq(1, N)] || _ <- lists:seq(1, M)],
    blockstripe:calculate(L, M, N, A, B),
    ok;
push_data(seq, L, M, N) ->
    A = [[rand:uniform(21)/7 || _ <- lists:seq(1, M)] || _ <- lists:seq(1, L)],
    B = [[rand:uniform(21)/7 || _ <- lists:seq(1, N)] || _ <- lists:seq(1, M)],
    element(1, timer:tc(matrix, dot, [A, B])).
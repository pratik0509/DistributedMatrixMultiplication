-module(blockstripe).
-export([start_server/0, start_mnode/1, calculate/5, server/1, combine/3, mnode/2, setvalue/2]).
-import(matrix, []).
-import(vector, []).
-import(helper, []).

server_node() ->
    pjain@localhost.

setvalue(Mat, []) ->
    Mat;
setvalue(Mat, [Head | Tail]) ->
    setvalue(matrix:set(element(2, Head)-1, element(3, Head)-1, element(1, Head), Mat), Tail).

split(_, _, _, _, _, _, _, []) ->
    [];
split(L, M, N, A, B, Nodes, NodeNum, ProductPoints) ->
    To = lists:nth(NodeNum + 1, Nodes),
    Point = hd(ProductPoints),
    NewProductPoints = tl(ProductPoints),
    I = element(1, Point),
    J = element(2, Point),
    Avec = matrix:row(A, I),
    Bvec = matrix:col(B, J),
    % io:format("To: ~w~n", [To]),
    {compute_node, element(2, To)} ! {Avec, Bvec, I, J},
    split(L, M, N, A, B, Nodes, (NodeNum + 1) rem length(Nodes), NewProductPoints).

combine(Result, Rows, Cols) ->
    Sz = dict:size(Result),
    if Rows * Cols ==  Sz ->
        eprof:stop_profiling(),
        eprof:analyze(),
        % io:format("Finished Computation!! ~w ~n", [lists:sort(dict:to_list(Result))]);
        io:format("Finished Computation!! ~n", []);
    true ->
        receive
            {result, Val, I, J} ->
                % NewResult = [Result | {Val, I, J}],
                NewResult = dict:store({I, J}, Val, Result),
                combine(NewResult, Rows, Cols)
        end
    end.

server(NodeList) ->
    receive
        {From, add_node, Name} ->
            NewNodeList = [{From, Name} | NodeList],
            io:format("New Node Added: ~n", []),
            server(NewNodeList);
        {multiply, L, M, N, A, B} ->
            io:format("Received Multiply Request: ~n", []),
            ProductPoints = helper:calculate_pairs(L, N),
            CPid = spawn(blockstripe, combine, [dict:new(), L, N]),
            register(combiner, CPid),
            io:format("Combiner Spawned ~n", []),
            eprof:start(),
            eprof:start_profiling([CPid]),
            split(L, M, N, A, B, NodeList, 0, ProductPoints), % To add the multiplication logic
            io:format("Split Finished ~n", []),
            server(NodeList)
    end.

start_server() ->
    io:format("Starting Server ~n", []),
    register(blockstripe_server, spawn(blockstripe, server, [[]])).


start_mnode(Name) ->
    io:format("Starting Node ~n", []),
    register(compute_node, spawn(blockstripe, mnode, [server_node(), Name])).

mnode(ServerNode, Name) ->
    {blockstripe_server, ServerNode} ! {self(), add_node, Name},
    io:format("Spawned Node ~n", []),
    mnode(ServerNode).

mnode(ServerNode) ->
    receive
        {AVec, BVec, I, J} ->
            AB = vector:dot(AVec, BVec),
            % io:format("Result ~w, ~w = ~w ~n", [I, J, AB]),
            {combiner, ServerNode} ! {result, AB, I, J}
    end,
    mnode(ServerNode).

calculate(L, M, N, A, B) ->
    io:format("Calculate Received ~n", []),
    {blockstripe_server, server_node()} ! {multiply, L, M, N, A, B}.
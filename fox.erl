-module(fox).
-export([start_server/0, start_mnode/1, calculate/4, server/1, combine/3, mnode/2, setvalue/2]).
-import(matrix, []).
-import(vector, []).
-import(helper, []).

server_node() ->
    'server@nirvan-HP-Pavilion-Notebook'.

setvalue(Mat, []) ->
    Mat;
setvalue(Mat, [Head | Tail]) ->
    setvalue(matrix:set(element(2, Head)-1, element(3, Head)-1, element(1, Head), Mat), Tail).

combine(Result, Rows, Cols) ->
    Sz = dict:size(Result),
    if Rows * Cols ==  Sz ->
        eprof:stop_profiling(),
        eprof:analyze(),
        io:format("Finished Computation!! ~w ~n", [lists:sort(dict:to_list(Result))]);
    true ->
        receive
            {From, result, Val} ->
                % NewResult = [Result | {Val, I, J}],  
                NewResult = dict:store({From}, Val, Result),
 
                combine(NewResult, Rows, Cols)
        end
    end.

server(NodeList) ->
    io:format("New Node Added: ~w ~n", [NodeList]),
    receive
        {From, add_node, Name} ->
            NewNodeList = [{From, Name} | NodeList],
            %io:format("New Node Added: ~w ~n", [NewNodeList]),
            server(NewNodeList);
        {From, multiply, N, A, B, P} ->
            %io:format("Received Multiply Request: ~w ~n ~w ~n", [A, B]),
            initialize( 0, N div P , A, B, P, NodeList),
            CPid = spawn(fox, combine, [dict:new(), P, P]),
            register(combiner,CPid),
            eprof:start(),
            eprof:start_profiling([CPid]),
            split( N div P , A, B, P,NodeList,0, P), % To add the multiplication logic
            %io:format("Split Finished ~n", []),
            server(NodeList)
    end.

start_server() ->
    io:format("Starting Server ~n", []),
    register(fox_server, spawn(fox, server, [[]])).


start_mnode(Name) ->
    io:format("Starting Node ~n", []),
    register(compute_node, spawn(fox, mnode, [server_node(), Name])).

mnode(ServerNode, Name) ->
    {fox_server, ServerNode} ! {self(), add_node, Name},
    io:format("Spawned Node ~n", []),
    mnode(ServerNode,[],[[]],1).



   
  
mnode(ServerNode, Amat, Cmat,Counter) ->
    if Counter == 0 ->
        {combiner, ServerNode} ! {self(), result, Cmat};
    true->
    receive
        {initialize, Ainit, Cinit,P} -> 
            %io:format("Got  inital A ~w ~w ~n", [Ainit,Cinit]),
            mnode(ServerNode, Ainit , Cinit,P);
        {add, Cadd} -> 
            %io:format("Result  = ~w ~w~n", [Cmat, Cadd]),
            mnode(ServerNode, Amat, matrix:add(Cmat,Cadd),Counter-1);
        {mult, Bmat, To} ->
            io:format("Result  = ~w ~w ~w~n", [Amat, Bmat, Cmat]),

            AB = matrix:dot(Amat, Bmat),

            %io:format("Result  = ~w ~n", [AB]),
            {compute_node, To} ! {add, AB },
            mnode(ServerNode, Amat, Cmat,Counter)

    end
end.


calculate( N, A, B, P) ->
    io:format("Calculate Received ~n", []),
    {fox_server, server_node()} ! {self(), multiply, N, A, B, P}.

initialize(_ , _,_,_,_,[])-> [];
initialize( NodeNum, M, A, B, P, Nodes)->
    if  NodeNum >= ( P *P )  -> 
        io:format("Initialization Finished ~n");
    true -> 
    To = lists:nth(NodeNum + 1, Nodes),
    Rownum = (NodeNum div P)+1,
    Colnum =  (NodeNum rem P) + 1,

    Amat = matrix:submatrix(A,Rownum, Colnum, M),
    Cmat = [ [0 || X<- lists:seq(1,M)] || Y<- lists:seq(1,M)],
    %io:format("To Rownum Colnum Amat Cmat ~w, ~w, ~w ,~w , ~w~n", [To, Rownum, Colnum,Amat,Cmat]),
    {compute_node, element(2, To)}  !   {initialize, Amat, Cmat,P},
    initialize(NodeNum + 1, M, A, B, P, Nodes)
    end.    


split(_, _, _, _, _, _, 0)-> 
        io:format("Splitting Finished ~n");
split(M , A, B, P,Nodes, NodeNum, Round)->
    if NodeNum >=  P*P -> 
        split(M, A, B,P, Nodes, 0, Round-1); 
    true -> 
    Rownum = (NodeNum div P)+1,
    Colnum =  (NodeNum rem P) + 1,
    NewCol = ((Colnum -1 + P -Round) rem P ) + 1,
    ToA = lists:nth(NodeNum + 1, Nodes),
    Ind = (P*(Rownum-1)+ NewCol-1) ,
    ToC = lists:nth(Ind + 1 , Nodes),
    Bmat = matrix:submatrix(B, Colnum, NewCol, M),
    %io:format("ToA Rownum Colnum NewCol Ind ToC Bmat ~w, ~w, ~w,~w, ~w ,~w , ~w~n", [ToA, Rownum, Colnum,NewCol, Ind, ToC,Bmat]),

    {compute_node, element(2, ToA)} ! {mult, Bmat, element(2,ToC) },
    split(M ,A,B, P, Nodes, NodeNum+1, Round)
    end.    







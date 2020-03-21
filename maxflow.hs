module MaxFlow 
(solveMaxFlow,MaxFlowNet,Graph,Vertex(Vertex)) where

import Data.List 


data Vertex = Vertex {
                          vertexLabel :: String
                        , vertexNeighbors :: [(String,Int)]
                        , vertexDistance :: Int
                        , vertexPredecessor :: String
                      } deriving (Show)

-- We define a graph as a list of vertexes.
-- Each vertex is a label, an adjacency list (neighbors),
-- a distance away from the root, and a predecessor label.
type Graph =  [Vertex] 
--- We will begin by modeling a graph as a list of vertexes which themselves hold the edges in adjacency lists.

--- Getters
-- Takes in a vertex, a list of vertexes, and returns true or false.
vertexInVertexes :: Vertex -> [Vertex] -> Bool
-- Given an empty list of vertices, return false.
vertexInVertexes _ [] = False
-- Reduce the list of vertexes to a bool.
-- If at least one vertex label in the list matches the input vertex label, the result will be true.
vertexInVertexes Vertex {vertexLabel = label} (x:y) = foldl (\ acc l -> vertexLabel l == label || acc) False (x:y)

labelinGraph::Graph -> String -> Bool
labelinGraph []  label = False
labelinGraph (x:y) label = label == vertexLabel x || labelinGraph y label


-----------------------------------------------------------------------------
-- Takes a graph, a list of strings, and outputs a list of vertexes.
graphVertexes :: Graph -> [String] ->  [Vertex]
-- Empty graph.
graphVertexes ( []) _ = []
graphVertexes ( (x:y)) [] =[]
graphVertexes ( (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)



--- checks if Neighbors list is valid 
checkvertexNeighbors :: Graph-> [(String,Int)] -> Bool
checkvertexNeighbors [] _ = error  "Empty graph" 
checkvertexNeighbors g  [] = True 
checkvertexNeighbors g  ((label,cost):xs) = labelinGraph g label &&  checkvertexNeighbors g xs


checkVertex [] _ = error "impossible"
checkVertex g (Vertex label [] d p) = True
checkVertex g (Vertex label n d p) = checkvertexNeighbors  g n

checkVertexs [] _ = True
checkVertexs g []  = True
checkVertexs g (x:xs) = checkVertex g x && checkVertexs g xs


-- add vertices in graph 2 not in graph 1 to graph 1
addOtherVertices::Graph -> Graph -> Graph
addOtherVertices [] [] = []
addOtherVertices g1 [] = g1
addOtherVertices g1 g2 = g1 ++ ( filter  (\(Vertex label n p d) -> length (graphVertexes g1 [label]) == 0  ) g2 )

--- check if a graph is valid
checkGraph [] = True
checkGraph g = checkVertexs g g && length (nub labels) == (length labels) where labels = map( \(Vertex label n d p) -> label)g
 
 

type Queue a = [a]
pushQueue :: Queue a -> a -> [a]
pushQueue []  item = [item]
pushQueue l  item =  l ++ [item] 


popQueue :: Queue a  ->(Queue a,a)
popQueue []  = error "Empty queue"
popQueue (x:xs) = (xs,x)

updateDistPred :: [Vertex] -> Int -> [Char] -> [Vertex]
updateDistPred [] _ _ = []
-- Go though each vertex and swap the current distance and predecessor labels with the new parameters.
updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _) -> Vertex label n dist predLabel) (x:y)


updateDistPredbylabel [] _ _ _ = []
-- Go though each vertex that matches a label and swap the current distance and predecessor labels with the new parameters.
updateDistPredbylabel g label dist predLabel = map (\ (Vertex vlabel n p d ) -> if  label  == vlabel then  (Vertex vlabel n predLabel dist) else (Vertex vlabel n p d )  ) g


bfs_helper :: Graph -> Graph -> [String] -> Graph -> Graph
bfs_helper inGraph [] seen outGraph = outGraph
bfs_helper inGraph queue seen outGraph =  bfs_helper inGraph new_queue new_seen new_outgraph
   where 
     -- get the vertex from the queue 
       (newqueue,(Vertex label neighbors distance pred)) = popQueue queue

       -- get the labels of the neighbors  of the vertex 
       neighborslabels =  map( \(vlabel,cost) -> vlabel) neighbors

      
       -- filter out vartices that were seen already labels
       neighborslabelsFiltered = filter (\l ->  not(elem l   seen)) neighborslabels
       
       -- Get the vertices of the set of non seen labels 
       vertexneighborslabelsFiltered  = graphVertexes inGraph neighborslabelsFiltered

       -- update the distance and predecessor for thoss vertices 
       new_vertices = updateDistPred vertexneighborslabelsFiltered (distance+1) label

       --add those vertices to the output graph 
       new_outgraph = outGraph ++ new_vertices

       --add the labels of those vertices to the seen set 
       new_seen= seen ++ neighborslabelsFiltered
       
       -- add those new new_vertices to the queue
       new_queue =  newqueue ++ new_vertices 

bfs inGraph startLabel = 
   let 
     -- Set the graph to initial values of distance and pred 
     inGraph_new=updateDistPredbylabel (updateDistPred inGraph (maxBound::Int) "")   startLabel "" 0
     
     -- Get the initial vertex 
     startVertex  =  (graphVertexes inGraph_new [startLabel])
     
     -- Initialize the queue and output graph
     queue = startVertex  
     outGraph = startVertex
     
     -- check the graph is valid
     ok = (checkGraph inGraph_new )
     
     -- set the seen list
     seen = [startLabel]
     
     -- Run BFS
     in if ok then addOtherVertices (bfs_helper inGraph_new queue seen outGraph) inGraph_new else error ("bad graph" ++ show inGraph_new)

no_path_mark = "no path"
trace_back_helper :: Graph -> [Char] -> [Char] -> [[Char]]
trace_back_helper _ "" endLabel = [no_path_mark]
trace_back_helper [] startLabel endLabel = []
trace_back_helper g startLabel endLabel = 
   let start_vertex_arr = (graphVertexes g [startLabel])
       start_vertex =   if length start_vertex_arr == 0 then error ("no  vertex  found " ++ startLabel ) else head start_vertex_arr
       pred = vertexPredecessor start_vertex
       
   in if startLabel == endLabel 
          then [startLabel] 
       else 
       if  pred == endLabel 
          then [startLabel,endLabel] 
       else [startLabel] ++ trace_back_helper g pred endLabel

trace_back :: [Vertex] -> [Char] -> [Char] -> [[Char]]
trace_back g startLabel endLabel = if elem no_path_mark path then [] else path where path=(trace_back_helper g startLabel endLabel )

--- Find the shortest path from source to destination 
bfs_path :: Graph-> [Char] -> [Char] -> [[Char]]
bfs_path [] startLabel endLabel   = [] 
bfs_path g startLabel endLabel = 
 let bfs_g = bfs g startLabel
     in  reverse(trace_back bfs_g endLabel startLabel)


g =  [
                Vertex "a" [("b",3), ("c",3) ,("d",7)         ] (maxBound::Int)  "",
                Vertex "b" [("d",6), ("f",5)     ] (maxBound::Int) "",
                Vertex "c" [("a",3) ,("e",3)        ] (maxBound::Int) ""    ,
                Vertex "d" [ ("a",5)] (maxBound::Int) ""      ,
                Vertex "e" [("c",4) ] (maxBound::Int) ""      ,
                Vertex "f" [ ] (maxBound::Int) ""                              
                ]

--- The network flow is a graph with source and sync 
data MaxFlowNet = MaxFlowNet {
                         graph :: Graph,
                         source:: String,
                         sync ::  String 
                      } deriving (Show)
 
--- test if neighbors contains a vertex label 
neighborscontains :: Eq t => [(t, b)] -> t -> Bool
neighborscontains [] l = False
neighborscontains ((nlabel,cost):xs) l = l == nlabel || neighborscontains xs l 
 

--- calculate the minimum cost along a path
minCostforPath :: Graph -> [String] -> Int
minCostforPath g [] = error "invalid path for minCostforPath -empty "
minCostforPath g [x] = (maxBound::Int)
minCostforPath g (x:y:xs)  = min xyCost restCost where 
                                                     (Vertex _ n p d ) = head $ filter (\ (Vertex label' n' p' d' ) -> label' == x) g
                                                     (label ,xyCost) = head $ filter (\ (label' ,cost') -> label' == y) n
                                                     restCost =   minCostforPath g (y:xs)


--- reduce  the cost of of an edge - if new cost is zero then remove that edge 
reduceCost'::[(String, Int)] -> String  -> Int -> [(String, Int)]                              
reduceCost' [] _ _ = []  
reduceCost' ((label,oldcost):xs) dest  amount  | label == dest && oldcost - amount < 0 = error "invalid cost update"
reduceCost' ((label,oldcost):xs) dest  amount  | label == dest && amount == oldcost =  xs 
reduceCost' ((label,oldcost):xs) dest  amount  | label == dest && amount < oldcost =  (label,oldcost-amount):xs
reduceCost' ((label,oldcost):xs) dest  amount  = (label,oldcost):(reduceCost xs dest amount)


--- reduce the cost of  an edge - if new cost is zero then remove that edge if it does not exist creat that edge  with the cost be the negative of the reduction 
reduceCost::[(String, Int)] -> String  -> Int -> [(String, Int)]  
reduceCost n dest amount = if exist  then reduceCost' n dest amount else n++[(dest,(-amount))] where exist =  length ( filter (\ (label' ,_) -> label' == dest) n ) > 0 

reduceEdgeCost :: Graph -> String -> String -> Int -> Graph
reduceEdgeCost g source dest reduceAmount   = newGraph  where 
                                              updatenewGraph = map (\(Vertex label' n' p' d' ) -> if label' == source then  (Vertex label' (reduceCost n' dest reduceAmount) p' d' ) else (Vertex label' n' p' d' ) ) g
                                              newGraph = if checkGraph updatenewGraph then updatenewGraph else error "invalid graph"
                                              
                                             
-- reduce cost along a path as part of the edmond karp algorithem 
reduceEdgeCostAlongPath:: Graph -> [String] -> Int -> Graph
reduceEdgeCostAlongPath graph [] reduceAmount = graph
reduceEdgeCostAlongPath graph [a] reduceAmount = graph
reduceEdgeCostAlongPath graph (x:y:xs) reduceAmount = newgraph where 
                                                                    g1 = reduceEdgeCost graph x y reduceAmount
                                                                    g2 = reduceEdgeCost g1 y x (-reduceAmount)
                                                                    newgraph = reduceEdgeCostAlongPath g2 (y:xs) reduceAmount
                                                    



solveMaxFlowStep:: Graph -> String -> String -> (Graph,Int)                      
solveMaxFlowStep  graph source sync = ret                     
 where 
     -- get the shortest path from source to sync  
     path = bfs_path graph source sync 
     
     -- get the minimum capacity along this path
     mincost = minCostforPath graph path 
          
     -- reduce the capacity along the path and ad residual capacity
     ret  = if path == [] then (graph,0) else  ((reduceEdgeCostAlongPath graph path mincost),mincost)
     


--- Helper function to solve the maxflow problem      
solveMaxFlow_helper:: Graph -> String -> String -> Int -> (Graph,Int)                      
solveMaxFlow_helper graph source sync currentflow = ret  
 where 
     -- single step of edmond karp
     (residual,flow) = solveMaxFlowStep graph source sync 
     
               
     -- repeat untill there is no path from source to sync 
     ret  = if flow == 0 then (graph,currentflow) else solveMaxFlow_helper  residual source sync (flow + currentflow)


--- Solve the maxflow problem. Input is a graph ,source and sync. Output is the saturated residual graph and the maxflow  
solveMaxFlow:: Graph -> String -> String ->(Graph,Int)                      
solveMaxFlow graph source sync = solveMaxFlow_helper  graph source sync 0  



g2 =  [
                Vertex "0" [("1",16), ("2",13)         ] (maxBound::Int)  "",
                Vertex "1" [("2",10), ("3",12)  ] (maxBound::Int) "",
                Vertex "2" [("4",14) ,("1",4)        ] (maxBound::Int) ""    ,
                Vertex "3" [ ("5",20), ("2",9)] (maxBound::Int) ""      ,
                Vertex "4" [("5",4), ("3",7) ] (maxBound::Int) ""      ,
                Vertex "5" [ ] (maxBound::Int) ""    
      ]

g3 =  [
                Vertex "0" [("1",4), ("4",2)         ] (maxBound::Int)  "",
                Vertex "1" [("3",2), ("2",4)  ] (maxBound::Int) "",
                Vertex "2" [("5",3)        ] (maxBound::Int) ""    ,
                Vertex "3" [ ("2",1), ("5",1)] (maxBound::Int) ""      ,
                Vertex "4" [("3",1), ("5",3) ] (maxBound::Int) ""      ,
                Vertex "5" [ ] (maxBound::Int) ""    
      ]

--- Test if networkflow graph is valid  
isValidMaxFlowNet :: MaxFlowNet -> Bool                     
isValidMaxFlowNet (MaxFlowNet g source sync) =  labelinGraph  g source &&  labelinGraph  g sync   && noEdgesIntoSource && noEdgesFromSync && source /= sync && checkGraph g
                                                    where 
                                                        noEdgesIntoSource = length (  filter (\(Vertex label n p d)  -> neighborscontains n source) g ) == 0
                                                        noEdgesFromSync = length (  filter (\(Vertex label n p d) -> label == sync && n /= []) g ) == 0


          

   


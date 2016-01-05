
{-# LANGUAGE GADTs, KindSignatures #-}
-- INDUCTIVE GRAPH  
-- Nodes are always represented by INT(for representing uniquenness of nodes in a easier way) 
-- Just a type definition for easier understanding
type Node = Integer

-- List of edges to a particular node 
-- b is the value of the edge (weighted graph)
-- Node represents the node to which the relative node is connected by the edge
type Adj b = [(Node,b)]


-- Given a list of context for each node in the graph then we can represent the graph.
-- Instead of doing a list of context,they use the "&" to make it easier in recursive/inductive function
-- Graphs are always built using the Context or the Empty Constructor
-- Graph can either be empty or it can be a Context(which represents a node and incoming&outgoing edges to that node) and a another graph.
-- Now you see the INDUCTIVE defintion and hence the name graph
-- Now coming to the type.Graphs are typed and are * -> * -> * 
-- The data type a is for the way the node is referenced (Eg : Int,Char).It should be unique
-- The data type b is to denote the weight of the edge. (Eg : Int,Char).This graph defintion assumes that we are dealing with weighted graph

-- Create a data constructor for context type
-- Similarly for Composite before we can construct Graph
data Context a b = Context (Adj b,Node,a,Adj b)
data Graph a b = Empty | Composite  (Context a b) (Graph a b) 
type WeightedGraph = Graph Int Int
				

instance (Show a,Show b) => Show (Graph a b) where
	show (Empty) = "Empty" 
	show (Composite context (nextNode))	= (show context) ++ "\n" ++ show  nextNode

instance (Show a,Show b) => Show (Context a b) where
	show (Context (incoming,nodeNumber,value,outgoing)) = "  Context -> N  " ++  show (nodeNumber) ++  "  V  "  ++ show (value) ++ " incoming edges " ++ show (incoming)  ++ " outgoing edges  " ++ show (outgoing) 
	
								  
-- Pick a particular Context using the Node identifier								 
getContext :: Node -> Graph a b -> Graph a b
getContext n (Composite (Context (incoming,nodeNumber,value,outgoing)) (nextNode)) = 
	if n==nodeNumber then (Composite (Context (incoming,nodeNumber,value,outgoing)) (Empty))
		else  getContext n nextNode  																							
getContext _ Empty = Empty									 

-- Return all the node Identifiers in the graph 
node::Graph a b -> [Node]
node (Composite (Context (_,nodeNumber,_,_)) (nextNode)) =  nodeNumber : (node nextNode)
node Empty = []

-- Sucessor 
suc::Graph a b -> [Node]
suc (Composite (Context (_,_,_,outgoingEdges)) (_)) = map fst outgoingEdges
suc _ = []

-- Sucessor with context
suc1 :: Context t b -> [Node]
suc1 ( (Context (_,_,_,outgoingEdges)) ) = map fst outgoingEdges


--For debugging
sucNode n graph = suc $ getContext n graph	 	
-- given a Node remove that node from the graph.. The reulting graph may not be complete and also the ADJ in graph could point to node whose context is not defined in the graph
removeContext::Node -> Graph a b -> Graph a b 
removeContext n (Composite (Context (incoming,nodeNumber,value,outgoing)) (nextNode)) = 
	if n==nodeNumber then nextNode 
		else Composite (Context (incoming,nodeNumber,value,outgoing)) (removeContext n nextNode)
removeContext n Empty = Empty

-- DEATH FIRST Search 
-- Takes a list of all nodeIdentifiers and the graph and gives the order of traversal in the graph as a list of nodeIdentifiers
-- child before sibling
-- The list input is to account for any unconnected nodes.(it is not necessary that all nodes can be reached from the child) 
-- We keep track of the visited nodes by removig that context from the graph.
-- We return empty list when the graph/list is empty 

dfs::[Node] -> WeightedGraph  -> [Node]
dfs (v:vs) graph = case (getContext v graph) of
					Empty  	->  dfs vs graph
					context ->   v : dfs (successors ++ (vs)) modifiedGraph
						where
							successors = (suc context) 
							modifiedGraph = removeContext v graph  	
dfs [] Empty = []
{-
Run it with 
dfs (node inputGraph2) (inputGraph2)
-}

-- BREATH FIRST SEARCH (BFS)
-- Takes a list of all nodeIdentifiers and an emptyList and the graph and gives the order of traversal in the graph as a list of nodeIdentifiers
-- sibling before child
-- The first list input is to account for any unconnected nodes.(it is not necessary that all nodes can be reached from the child) 
-- The empty list in used to keep track of the queue 
-- We keep track of the visited nodes by removig that context from the graph.
-- We return empty list when the graph/list is empty  
bfs::[Node] -> WeightedGraph  -> [Node]
bfs    _       Empty       = []
bfs (v:vs) (Composite c g) = v:(bfs (vs++suc1 c ) g)
{-
Run it with
bfs (node inputGraph2) (inputGraph2)
-}

--Examples
--edges
{-
consider the graph where the edge detials for the 6 nodes are
there is a edge from these nodes to the noder

1 -> 2 , 3 , 4
3 -> 6
6 -> 2 , 5
5 -> 4
-}
p1=(2,0)
p2=(3,0)
p3=(4,0)
p4=(6,0)
p5=(2,0)
p6=(5,0)
p7=(4,0)
--node details
d1= Composite (Context ([],1,0,[p1,p2,p3]))
d2=Composite (Context ([p1,p5],2,0,[]))
d3=Composite (Context ([p2],3,0,[p4]))
d4=Composite (Context ([p3,p7],4,0,[]))
d5=Composite (Context ([p6],5,0,[p7]))
d6=Composite (Context ([p4],6,0,[p5,p6]))

--Graph structure
inputGraph2 :: WeightedGraph								 
inputGraph2 = d1 $ 
					d2 $ 
						 d3 $ 
								d4 $ 
										d5 $ 
												d6 $ 
														Empty
														



 






						
									
 

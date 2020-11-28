module BDD where

import Data.List
import Data.Map as M hiding (map)
import Data.Maybe as Y

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x
  = snd . head . dropWhile (\p -> (fst p) /= x)

checkSat :: BDD -> Env -> Bool
checkSat (root, nodes) env
  = check root where
  check 0 = False
  check 1 = True
  check nodeid = check (choose node) where
    node = lookUp nodeid nodes
    choose (index, f, t) = if (lookUp index env) then t else f

sat :: BDD -> [[(Index, Bool)]]
sat (root, nodes)
  = sat' root where
  sat' 0 = []
  sat' 1 = [[]]
  sat' nodeid = map ((index, False) :) (sat' f) ++ map ((index, True) :) (sat' t) where
    (index, f, t) = lookUp nodeid nodes

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))
  = Prim (not b)
simplify (And (Prim b) (Prim b2))
  = Prim (b && b2)
simplify (Or (Prim b) (Prim b2))
  = Prim (b || b2)
simplify bexp = bexp

restrict :: BExp -> Index -> Bool -> BExp
restrict bexp index b
  = restrict' bexp where
  restrict' (IdRef i)
    | i == index = Prim b
    | otherwise  = IdRef i
  restrict' (Not bexp) = simplify (Not (restrict' bexp))
  restrict' (And bexp bexp2) = simplify (And (restrict' bexp) (restrict' bexp2))
  restrict' (Or bexp bexp2) = simplify (Or (restrict' bexp) (restrict' bexp2))
  restrict' bexp = bexp

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD (Prim b) _
  = (if b then 1 else 0, [])
buildBDD bexp indexes
  = buildBDD' bexp 2 indexes

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' bexp root indexes
  = (root, buildBDD'' bexp root indexes) where
  buildBDD'' _ _ []            = []
  buildBDD'' bexp nodeid (index : indexes) = (nodeid, (index, left, right))
                                             : (buildBDD'' lexp left indexes
                                                 ++ buildBDD'' rexp right indexes) where
    left  = choose lexp (2 * nodeid)
    right = choose rexp (2 * nodeid + 1)
    lexp  = restrict bexp index False
    rexp  = restrict bexp index True
    choose (Prim b) _ = if b then 1 else 0
    choose _ n = n

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD bexp indexes = applyEliminate (applyShare (buildBDD bexp indexes))

applyEliminate (root, allNodes) = (root, applyEliminate' root) where
  applyEliminate' 0 = []
  applyEliminate' 1 = []
  applyEliminate' nodeid =
    let
      (nodeid', node) = eliminate nodeid allNodes
      (index, left, right) = fromMaybe (lookUp nodeid allNodes) node
      leftNodes = applyEliminate' left
      rightNodes = applyEliminate' right
    in
      ((if nodeid /= root then nodeid' else root, (index, left, right)) : (leftNodes ++ rightNodes))

eliminate nodeid nodes
  | nodeid == 0 = (0, Nothing)
  | nodeid == 1 = (1, Nothing)
  | otherwise =
    let
      (leftChild', leftTriple) = eliminate leftChild nodes
      (rightChild', rightTriple) = eliminate rightChild nodes
      (index, leftChild, rightChild) = lookUp nodeid nodes
    in
      if leftChild' == rightChild' then
        (leftChild', leftTriple)
      else
        (nodeid, Just (index, leftChild', rightChild'))

applyShare (root, allNodes) = (root, (fst (applyShare' M.empty root))) where
  subs = subtrees allNodes
  applyShare' memo 0 = ([], memo)
  applyShare' memo 1 = ([], memo)
  applyShare' memo nodeid =
    let
      (index, leftChild, rightChild) = lookUp nodeid allNodes
      (children, memo') = subtreeMemo memo nodeid allNodes subs
      (leftChild', rightChild') = fromMaybe (leftChild, rightChild) children
      (left, memo'') = applyShare' memo' leftChild'
      (right, memo''') = applyShare' memo'' rightChild'
    in
      (((nodeid, (index, leftChild', rightChild')) : (left ++ right)), memo''')

subtreeMemo memo nodeid nodes subs
  | nodeid == 0 = (Nothing, memo)
  | nodeid == 1 = (Nothing, memo)
  | otherwise =
    let
      (index, leftChild, rightChild) = lookUp nodeid nodes
      (leftChild', memo')
        | leftChild == 0 || leftChild == 1 = (Nothing, memo)
        | otherwise = M.insertLookupWithKey (\k n o -> o) leftTree leftChild memo where
          leftTree = lookUp leftChild subs
      (rightChild', memo'')
        | rightChild == 0 || rightChild == 1 = (Nothing, memo')
        | otherwise = M.insertLookupWithKey (\k n o -> o) rightTree rightChild memo' where
          rightTree = lookUp rightChild subs
    in
      (Just (fromMaybe leftChild leftChild', fromMaybe rightChild rightChild'), memo'')

subtrees :: [BDDNode] -> [BDD]
subtrees nodes = map subtree nodes where
  subtree (nodeid, (index, left, right)) = (nodeid, subtree' nodeid 2)
  subtree' 0 _ = []
  subtree' 1 _ = []
  subtree' nodeid nodeid' = (nodeid', (index, left', right')) : (subtree' left left' ++ subtree' right right') where
    left'
      | left == 0 = left
      | left == 1 = left
      | otherwise = 2 * nodeid'
    right'
      | right == 0 = right
      | right == 1 = right
      | otherwise = 2 * nodeid' + 1
    (index, left, right) = lookUp nodeid nodes

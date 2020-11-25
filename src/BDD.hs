module BDD where

import Data.List
import Data.Maybe

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
buildROBDD bexp indexes = applyEliminate (buildBDD bexp indexes)

applyEliminate (root, allNodes) = (root, mapMaybe applyEliminate' allNodes) where
  applyEliminate' (nodeid, (index, left, right)) =
    let
      (nodeid', (index', left', right')) = eliminate nodeid allNodes
    in
      if nodeid' == 0 || nodeid' == 1 then
        Nothing
      else
        Just (nodeid', (index', left', right'))

eliminate nodeid nodes
  | nodeid == 0 = (0, (0, 0, 0))
  | nodeid == 1 = (1, (0, 1, 1))
  | otherwise =
    let
      (leftChild', leftTriple) = eliminate leftChild nodes
      (rightChild', rightTriple) = eliminate rightChild nodes
      (index, leftChild, rightChild) = lookUp nodeid nodes
    in
      if leftChild' == rightChild' then
        (leftChild', leftTriple)
      else
        (nodeid, (index, leftChild', rightChild'))

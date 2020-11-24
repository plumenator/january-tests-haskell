module BDD where

import Data.List

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
simplify 
  = undefined

restrict :: BExp -> Index -> Bool -> BExp
restrict 
  = undefined

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD 
  = undefined

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' 
  = undefined

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

module Data.DiGraph(Graph (), empty, create, addNode, addAllNodes, addNeighbors, addAllNeighbors, contains, getNeighbors, getNodes, isEmpty, hasCycle, generateInverseDependencyList) where

import qualified Data.Map as M
import qualified Data.Set as S

data (Ord a) => Graph a = Graph { nodes :: S.Set a
                                , next  :: M.Map a (S.Set a)
                                } deriving (Eq, Show)

empty :: Ord a => Graph a
empty = Graph { nodes = S.empty
              , next = M.empty
              }

create :: Ord a => [(a, [a])] -> Graph a
create pairs = addAllNeighbors pairs $ addAllNodes (map fst pairs) empty

addNode :: Ord a => a -> Graph a -> Graph a
addNode v graph = graph { nodes = v `S.insert` nodes graph }

addAllNodes :: Ord a => [a] -> Graph a -> Graph a
addAllNodes nodes graph = foldl (\g n -> addNode n g) graph nodes

addNeighbors :: Ord a => a -> [a] -> Graph a -> Graph a
addNeighbors v neighboors graph | graph `contains` v = graph { nodes = nodes $ addAllNodes neighboors graph, next = M.insert v (S.fromList neighboors) $ next graph }
                                | otherwise = addNeighbors v neighboors $ addNode v graph

addAllNeighbors :: Ord a => [(a, [a])] -> Graph a -> Graph a
addAllNeighbors pairs graph = foldl (\g (v, next) -> addNeighbors v next g) graph pairs

contains :: Ord a => Graph a -> a -> Bool
contains graph v = v `S.member` nodes graph

getNeighbors :: Ord a => Graph a -> a -> Maybe [a]
getNeighbors graph v = fmap S.toList $ M.lookup v $ next graph

getNodes :: Ord a => Graph a -> [a]
getNodes = S.toList . nodes

isEmpty :: Ord a => Graph a -> Bool
isEmpty graph = S.null $ nodes graph

hasCycle :: Ord a => Graph a -> Bool
hasCycle graph | isEmpty graph = False
               | otherwise = any (`go` S.empty) $ getNodes graph
  where
    go node visited | node `S.member` visited = True
                    | otherwise = case getNeighbors graph node of
                                    Nothing -> False
                                    Just next -> any (flip go $ node `S.insert` visited) next


generateInverseDependencyList :: Graph a -> (a -> [a]) -> [a]
generateInverseDependencyList graph depFun = undefined

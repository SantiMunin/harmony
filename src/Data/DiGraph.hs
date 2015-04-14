module Data.DiGraph( Graph ()
                   , empty
                   , create
                   , addNode
                   , addAllNodes
                   , addNeighbors
                   , addAllNeighbors
                   , contains
                   , deleteNode
                   , getNeighbors
                   , getNodes
                   , isEmpty
                   , hasCycle
                   , generateInverseDependencyList) where

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

deleteNode :: Ord a => a -> Graph a -> Graph a
deleteNode v graph = graph { nodes = v `S.delete` nodes graph
                           , next = deleteNodeFromNext v $ next graph}
  where
    deleteNodeFromNext v m = rmFromAllSets v $ M.delete v m
    rmFromAllSets v = M.map (S.delete v)

addAllNodes :: Ord a => [a] -> Graph a -> Graph a
addAllNodes nodes graph = foldl (flip addNode) graph nodes

addNeighbors :: Ord a => a -> [a] -> Graph a -> Graph a
addNeighbors v neighboors graph | graph `contains` v = graph { nodes = nodes $ addAllNodes neighboors graph, next = M.insert v (S.fromList neighboors) $ next graph }
                                | otherwise = addNeighbors v neighboors $ addNode v graph

addAllNeighbors :: Ord a => [(a, [a])] -> Graph a -> Graph a
addAllNeighbors pairs graph = foldl (\g (v, next) -> addNeighbors v next g) graph pairs

contains :: Ord a => Graph a -> a -> Bool
contains graph v = v `S.member` nodes graph

getNeighbors :: Ord a => Graph a -> a -> Maybe [a]
getNeighbors graph v = do
  ns <- M.lookup v $ next graph
  let nList = S.toList ns
  if null nList
  then Nothing
  else return nList

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

dfs :: Ord a => Graph a -> Maybe a
dfs graph | isEmpty graph = Nothing
          | otherwise = go (head $ getNodes graph) graph
  where
    go :: Ord a => a -> Graph a -> Maybe a
    go node graph =
      case getNeighbors graph node of
        Nothing -> return node
        Just [] -> error "getNeighbors returning \"Just []\" should never happen"
        Just (node':_) -> go node' graph

generateInverseDependencyList :: Ord a => Graph a -> [a]
generateInverseDependencyList graph = go graph []
  where
    go graph accum =
      case dfs graph of
        Nothing -> reverse accum
        Just n -> go (deleteNode n graph) (n:accum)

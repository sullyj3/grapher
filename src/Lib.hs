{-# LANGUAGE OverloadedLists #-}

module Lib where


import Data.Graph
import Data.Array


graphAddEdge :: Vertex -> Vertex -> Graph -> Graph
graphAddEdge u v g = g // [(u, newUSuccessors)]
  where
  uSuccessors = g ! u
  newUSuccessors = if v `elem` uSuccessors
                     then uSuccessors
                     else v:uSuccessors

-- Create new graph with n nodes and no edges
nNodesNoEdges :: Int -> Graph
nNodesNoEdges n = buildG (0,n) []

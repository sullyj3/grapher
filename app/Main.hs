{-# LANGUAGE LambdaCase, OverloadedStrings #-}

{-
 - Simple repl which allows you to create and manipulate graphs with integer vertices
 - Created to teach myself Megaparsec and StateT
 -}

module Main where

import Lib
import CommandParser

import qualified Data.Text.IO as TIO
import Data.String
import Control.Monad (guard)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Foldable (mapM_)

import Data.Graph
import Data.Array
import Data.Ix (inRange)
import Data.Maybe (fromJust)

import System.Console.Haskeline
import System.Exit (exitSuccess)

type Grapher = StateT Graph (InputT IO)

main :: IO ()
main = do
  initialGraph <- runInputT defaultSettings initGraph

  runInputT defaultSettings $ execStateT repl initialGraph
  return ()

initGraph :: InputT IO Graph
initGraph = do
  outputStrLn "How many nodes?"
  numNodes <- fmap (read . fromJust) (getInputLine "> ")
  outputStrLn $ "Creating new graph with " ++ show numNodes ++ " nodes."
  let initialGraph = nNodesNoEdges numNodes
  return initialGraph

repl :: StateT Graph (InputT IO) ()
repl = do
  -- todo: remove fromjust
  line <- fmap fromJust $ lift $ getInputLine "> "
  case getCommand $ fromString line of
    Left e        -> lift $ outputStrLn "invalid command"
    Right command -> handleCommand command
  repl

handleCommand :: Command -> Grapher ()
handleCommand (AddEdge u v) = do
  g <- get

  let uValid = vertexValid u g
  let vValid = vertexValid v g

  if (uValid && vValid)
    then do
      modify $ graphAddEdge u v
      showGraph
    else do
      lift $ outputStrLn "Bad vertices"
handleCommand ShowGraph = do
  showGraph
handleCommand Exit = liftIO exitSuccess

vertexValid :: Vertex -> Graph -> Bool
vertexValid u g = inRange (bounds g) u

showGraph :: Grapher ()
showGraph = do 
  g <- get
  lift $ mapM_ (outputStrLn . show) $ assocs g

prompt s = do
  TIO.putStr s
  readLn

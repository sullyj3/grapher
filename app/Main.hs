{-# LANGUAGE LambdaCase, OverloadedStrings #-}

{-
 - Simple repl which allows you to create and manipulate graphs with integer vertices
 - Created to teach myself Megaparsec and StateT
 -}

module Main where

import Lib
import CommandParser

import qualified Data.Text.IO as TIO
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Data.Foldable (mapM_)

import Data.Graph
import Data.Array

type GraphState = StateT Graph IO

main :: IO ()
main = do
  TIO.putStrLn "How many nodes?"
  numNodes <- prompt "> "
  putStrLn $ "Creating new graph with " ++ show numNodes ++ " nodes."
  let initialGraph = nNodesNoEdges numNodes

  runStateT repl initialGraph
  return ()

repl :: GraphState ()
repl = do
  liftIO $ putStr "> "
  -- TODO: better line editor
  line <- liftIO TIO.getLine
  case getCommand line of
    Left e -> do
      liftIO $ putStrLn "invalid command"
      repl
    Right command -> handleCommand command

handleCommand :: Command -> GraphState ()
handleCommand (AddEdge u v) = do
  -- TODO: validate
  modify $ graphAddEdge u v
  showGraph
  repl
handleCommand ShowGraph = do
  showGraph
  repl
handleCommand Exit = return ()

showGraph :: GraphState ()
showGraph = do 
  g <- get
  liftIO . mapM_ print . assocs $ g

prompt s = do
  TIO.putStr s
  readLn

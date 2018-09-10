{-# LANGUAGE OverloadedStrings #-}

module CommandParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Void
import Data.Text
import Data.Graph

data Command = Exit | AddEdge Vertex Vertex | ShowGraph
  deriving Show

type CommandParser = Parsec Void Text Command

parseCommand :: CommandParser
parseCommand = parseExit <|>
               parseAddEdge <|>
               parseShowGraph

parseExit :: CommandParser
parseExit = do
  string "exit"
  eof
  return Exit

parseAddEdge :: CommandParser
parseAddEdge = do
  string "edge"
  space
  u <- Lex.decimal
  space
  v <- Lex.decimal
  eof

  return $ AddEdge u v

parseShowGraph :: CommandParser
parseShowGraph = do
  string "show"
  eof
  return ShowGraph
--

getCommand input = runParser parseCommand "" input

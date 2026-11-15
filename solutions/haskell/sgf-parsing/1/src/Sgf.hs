{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = M.Map T.Text [T.Text]

parseSgf :: T.Text -> Maybe (Tree (M.Map T.Text [T.Text]))
parseSgf = parseMaybe gameTree

property :: Parser (T.Text, [T.Text])
property = (,) <$> T.pack <$> some upperChar <*> some (char '[' *> sgfText <* char ']')

sgfText :: Parser T.Text
sgfText = T.concat <$> (some $ choice
  [ char '\\' *> choice
      [ char '\n' *> pure T.empty
      , spaceChar *> pure " "
      , T.singleton <$> anySingle ]
  , T.singleton <$> newline
  , T.singleton <$> spaceChar *> pure " "
  , T.singleton <$> noneOf [']', '\\', ':']
  , spaceChar *> pure " " ])

nodeStarter :: Parser SgfTree
nodeStarter = char ';' *> (Node <$> (M.fromList <$> many property) 
                                <*> choice [ (:[]) <$> nodeStarter
                                           , many gameTree ]
                          )

gameTree :: Parser SgfTree
gameTree = char '(' *> nodeStarter <* char ')'
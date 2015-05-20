{-# LANGUAGE OverloadedStrings #-}
module Text.Editor.Keybinding where

import           Prelude hiding (Either(..))
import qualified Prelude as P

import           Control.Applicative
import           Control.Monad (msum)

import           Data.Automaton
import           Data.Process

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import           Language.Haskell.TH (Exp,Q)
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

import           Text.Editor.Keyboard (Key,Modifier,KeyCombination)
import qualified Text.Editor.Keyboard as K

keybindings :: Lift action => Parser action -> QuasiQuoter
keybindings parseAction = QuasiQuoter
  { quoteExp  = quoteBindings parseAction
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }

type KeyBinding action = ([KeyCombination],action)

binding :: Parser action -> Parser (KeyBinding action)
binding parseAction = do
  A.skipSpace
  keyCombinations <- (K.keyCombination <* A.skipSpace) `A.manyTill` A.string "->"
  A.skipSpace
  act <- parseAction
  return (keyCombinations,act)

bindings :: Parser action -> Parser [KeyBinding action]
bindings parseAction = binding parseAction `A.sepBy1` A.endOfLine
  
quoteBindings :: Lift action => Parser action -> String -> Q Exp
quoteBindings parseAction str = 
  case A.parseOnly (bindings parseAction) (T.pack str) of
    P.Left l  -> fail l
    P.Right r -> lift r

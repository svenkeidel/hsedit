{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Editor.VimActions where

import           Control.Applicative
import           Control.Monad (msum)

import           Prelude hiding (Either(..))
import qualified Prelude as P

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import           Language.Haskell.TH (Exp,Q)
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

import           Text.Editor.Keybinding

vimBindings :: QuasiQuoter
vimBindings = keybindings vimAction

data VimAction = MoveCursor Direction
  deriving Show

instance Lift VimAction where
  lift (MoveCursor dir) = [e| MoveCursor $(lift dir) |]

data Direction = Left | Right | Up | Down | BeginningOfFile | EndOfFile
  deriving Show

instance Lift Direction where
  lift Left = [e| Left |]
  lift Right = [e| Right |]
  lift Up = [e| Up |]
  lift Down = [e| Down |]
  lift BeginningOfFile = [e| BeginningOfFile |]
  lift EndOfFile = [e| EndOfFile |]

vimAction :: Parser VimAction
vimAction =
  moveCursor

moveCursor :: Parser VimAction
moveCursor = A.string "move cursor " *> (MoveCursor <$> direction)

direction :: Parser Direction
direction = 
  msum
  [ A.string "left" *> pure Left
  , A.string "right" *> pure Right
  , A.string "up" *> pure Up
  , A.string "down" *> pure Down
  , A.string "to the beginning of the file" *> pure BeginningOfFile
  , A.string "to the end of the file" *> pure EndOfFile
  ]

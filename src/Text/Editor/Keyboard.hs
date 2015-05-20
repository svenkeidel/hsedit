{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Editor.Keyboard
  ( Modifier(..)
  , Key (..)
  , KeyCombination
  , keys
  , keyCombination
  ) where

import           Prelude hiding (Either(..))
import qualified Prelude as P

import           Control.Applicative
import           Control.Monad (msum,unless)

import           Data.Word (Word8)
import           Data.List (intersperse)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

data KeyCombination
  = KeyCombination (Set Modifier) Key

instance Show KeyCombination where
  show (KeyCombination mods k) 
    | S.null mods = show k
    | otherwise   = showMods ++ "-" ++ show k
    where
      showMods = concat $ intersperse "-" $ show <$> S.toList mods

instance Lift KeyCombination where
  lift (KeyCombination mods k) =
    [e| KeyCombination (S.fromList $(lift (S.toList mods))) $(lift k) |]

data Modifier
    = Control
    | Meta
    | Shift
    | Mod4
    deriving (Ord, Eq)

instance Show Modifier where
  show Control = "C"
  show Meta = "M"
  show Shift = "S"
  show Mod4 = "M4"

instance Lift Modifier where
  lift Control = [e| Control |]
  lift Meta = [e| Meta |]
  lift Shift = [e| Shift |]
  lift Mod4 = [e| Mod4 |]

data Key
    = Character Char
    | Backspace
    | Delete
    | Function Word8
    | Escape
    | PageUp
    | PageDown
    | Enter
    | Tabulator
    | Up
    | Left
    | Right
    | Down
    | PrintScreen
    | Pause
    | Insert
    | Home
    | Begin
    | End
    | Menu
    deriving Eq

instance Lift Key where
  lift (Character c) = [e| Character $(lift c) |]
  lift Backspace = [e| Backspace |]
  lift Delete = [e| Delete |]
  lift (Function n) = [e| Function $(lift (fromIntegral n :: Int)) |]
  lift Escape = [e| Escape |]
  lift PageUp = [e| PageUp |]
  lift PageDown = [e| PageDown |]
  lift Enter = [e| Enter |]
  lift Tabulator = [e| Tabulator |]
  lift Up = [e| Up |]
  lift Left = [e| Left |]
  lift Right = [e| Right |]
  lift PrintScreen = [e| PrintScreen |]
  lift Pause = [e| Pause |]
  lift Insert = [e| Insert |]
  lift Home = [e| Home |]
  lift Begin = [e| Begin |]
  lift End = [e| End |]
  lift Menu = [e| Menu |]

instance Show Key where
  show (Character c) = [c]
  show (Function n)  = "<F" ++ show n ++ ">"
  show Backspace     = "<BS>"
  show Delete        = "<Del>"
  show Escape        = "<Esc>"
  show PageUp        = "<PageUp>"
  show PageDown      = "<PageDown>"
  show Enter         = "<Enter>"
  show Tabulator     = "<Tab>"
  show Up            = "<Up>"
  show Left          = "<Left>"
  show Right         = "<Right>"
  show Down          = "<Down>"
  show PrintScreen   = "<PrintScreen>"
  show Pause         = "<Pause>"
  show Insert        = "<Insert>"
  show Home          = "<Home>"
  show Begin         = "<Begin>"
  show End           = "<End>"
  show Menu          = "<Menu>"

parseShortSyntax :: Text -> P.Either String [KeyCombination]
parseShortSyntax = A.parseOnly (A.skipSpace *> shortSyntax <* A.skipSpace)

shortSyntax :: Parser [KeyCombination]
shortSyntax = keyCombination `A.sepBy1` A.space

modifier :: Parser Modifier
modifier =
  msum
  [ A.char 'S' *> pure Shift
  , A.char 'C' *> pure Control
  , A.char 'M' *> pure Meta
  ]

keyCombination :: Parser KeyCombination
keyCombination = do
  mods <- A.many' $ modifier <* A.char '-'
  KeyCombination (S.fromList mods) <$> key

functionKey :: Parser Key
functionKey = do
  A.char '<'
  A.char 'F' <|> A.char 'f'
  dec <- A.decimal
  unless (dec >= 0 && dec <= 12) $ fail ("<F" ++ show dec ++ "> is not a valid function key")
  A.char '>'
  return $ Function dec

key :: Parser Key
key =
  msum
  [ functionKey
  , A.string "<BS>"          *> pure Backspace
  , A.string "<Del>"         *> pure Delete
  , A.string "<Esc>"         *> pure Escape
  , A.string "<PageUp>"      *> pure PageUp
  , A.string "<PageDown>"    *> pure PageDown
  , A.string "<Enter>"       *> pure Enter
  , A.string "<Tab>"         *> pure Tabulator
  , A.string "<Up>"          *> pure Up
  , A.string "<Left>"        *> pure Left
  , A.string "<Right>"       *> pure Right
  , A.string "<Down>"        *> pure Down
  , A.string "<PrintScreen>" *> pure PrintScreen
  , A.string "<Pause>"       *> pure Pause
  , A.string "<Insert>"      *> pure Insert
  , A.string "<Home>"        *> pure Home
  , A.string "<Begin>"       *> pure Begin
  , A.string "<End>"         *> pure End
  , A.string "<Menu>"        *> pure Menu
  , A.string "<Space>"       *> pure (Character ' ')
  , Character <$> A.anyChar
  ]

keys :: QuasiQuoter
keys = QuasiQuoter
  { quoteExp  = quoteKeys
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }

quoteKeys :: String -> Q Exp
quoteKeys str =
  case parseShortSyntax (T.pack str) of
    P.Left l  -> fail l
    P.Right r -> lift r

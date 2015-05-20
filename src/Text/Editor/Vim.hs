{-# LANGUAGE QuasiQuotes #-}
module Text.Editor.Vim where

import           Text.Editor.Keybinding
import           Text.Editor.VimActions

vim :: [KeyBinding VimAction]
vim = [vimBindings|
  h   -> move cursor left
  j   -> move cursor down
  k   -> move cursor up
  l   -> move cursor right
  g g -> move cursor to the beginning of the file
  G   -> move cursor to the end of the file
|]

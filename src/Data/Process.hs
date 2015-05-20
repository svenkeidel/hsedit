module Data.Process where

import           Data.Automaton (Automaton)
import qualified Data.Automaton as A
import           Data.Map (Map)
import qualified Data.Map as M

-- | An automaton is just the scheme for a process. A process is an automaton
-- bundled with a state.
data Process state input output = Process
  { state       :: state
  , transitions :: Map (state,input) (state,output)
  } deriving (Eq,Show)

start :: (Ord s,Ord i) => Automaton s i o -> Process s i o
start auto = Process
  { state       = A.initialState auto
  , transitions = M.fromList [ ((s,i),(s',o)) | (s,i,s',o) <- A.transitions auto ]
  }

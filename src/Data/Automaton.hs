module Data.Automaton where

data Automaton state input output = Automaton
  { initialState :: state
  , transitions  :: [(state,input,state,output)]
  } deriving (Eq,Show)

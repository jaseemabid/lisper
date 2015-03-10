module Lisper.Util (debug) where

import Debug.Trace (trace)

-- Debug helpers
debug :: c -> String -> c
debug = flip trace

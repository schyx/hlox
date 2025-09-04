module Phases.Scanner (scanTokens, ScanResult) where

import Tokens

type ScanResult = Either [String] [Token]

-- | Implements scanning phase of the interpreter
scanTokens :: String -> ScanResult
scanTokens _ = Right []

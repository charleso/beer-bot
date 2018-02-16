{-# LANGUAGE NoImplicitPrelude #-}

import           Beer.Prelude

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
    ] >>= \rs -> when (not . all id $ rs) exitFailure

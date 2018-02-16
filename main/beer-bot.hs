{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Nest

import qualified Network.Linklater as Linklater

import           System.IO (IO)
import qualified System.IO as IO

import qualified Beer
import           Beer.Prelude

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 8888
  hook <- Nest.force $ Nest.string "HOOK"
  IO.putStrLn ("[beer-bot] listening on port: " <> show port)
  Warp.run port (Linklater.slashSimple $ \c -> Beer.beer (Linklater.Config hook) c >> pure "")

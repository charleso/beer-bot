{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Beer (
    beer
  ) where

import           Beer.Prelude

import qualified Control.Concurrent as Concurrent
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Catch as E
import           Control.Lens ((^..), (^?!), to, view)

import           Data.Aeson (Value, (.=), encode, object)
import           Data.Aeson.Lens (_Array, _Double, _String, key)
import           Data.Char (isLetter)
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.HTTP.Client (HttpException)
import qualified Network.Linklater as Linklater
import           Network.Wreq

import qualified System.IO as IO


data Beers =
  Beers {
      _beersVenueId :: VenueId
    , _beersResult :: [Beer]
    } deriving (Eq, Show)

data Beer =
  Beer {
      _beerName :: Text
    , _breweryName :: Text
    , _beerStyle :: Text
    , _beerAbv :: Double
    } deriving (Eq, Show)

newtype VenueId =
  VenueId {
      unVenueId :: Double
    } deriving (Eq, Show)

-----------------

beer :: Linklater.Config -> Linklater.Command -> IO Linklater.Message
beer config (Linklater.Command _name _user channel msg) = do
  -- Yuck. Ideally we should just return the message but Untapped can take
  -- longer than 3 seconds which is slack's hard timeout on the initial response
  (_ :: Concurrent.ThreadId) <- Concurrent.forkIO $ case msg of
    Nothing ->
      pure ()
    Just msg' -> do
      e1 <- runEitherT $ searchBeers msg'
      e <- runEitherT . flip Linklater.say config $ case e1 of
        Left e2 ->
          -- FIX A better way to display errors?
          beerMsgStub channel . T.pack . show $ e2
        Right beers ->
          beerMsgStub channel . beerMsg $ beers
      case e of
        Left err -> do
          IO.print err
        Right () -> do
          pure ()
  -- Return immediately to get the original command to print
  pure $
    beerMsgStub channel ""

-----------------

beerMsgStub :: Linklater.Channel -> Text -> Linklater.Message
beerMsgStub c =
  Linklater.SimpleMessage (Linklater.EmojiIcon ":beer:") "beer" c Linklater.InChannel

beerMsg :: Beers -> Text
beerMsg (Beers venueId beers) =
  T.unlines $
    fmap renderBeer beers <>
      ["https://nowtapped.com/embed.html?venue=" <> (T.pack . show . unVenueId) venueId]

-- FIX Formatted message?
renderBeer :: Beer -> Text
renderBeer (Beer n bn s a) =
  n <> " by " <> bn <> " (" <> s <> " @ " <> T.pack (show a) <> "%)"

-----------------

searchBeers :: Text -> EitherT Text IO Beers
searchBeers location = do
  venueId <- venue location >>= maybe (left "No venue found") pure
  beers <- getBeers 20 venueId
  pure $ Beers venueId beers

-- Doesn't actually search - the key has to be exact :(
venue :: Text -> EitherT Text IO (Maybe VenueId)
venue location = do
  -- NOTE: 'slug' has to be exact - the web page actually loads _all_ the venues once and searches locally :(
  let slug = T.filter isLetter . T.filter (/= ' ') . T.toLower $ location
  r <- ntPost "/Venues" ["where" .= object ["slug" .= slug]]
  pure . listToMaybe $ r ^.. key "results" . _Array . traverse
    . to (\o -> VenueId $ o ^?! key "untappdId" . _Double)

getBeers :: Int -> VenueId -> EitherT Text IO [Beer]
getBeers limit (VenueId venueId) = do
  r <- ntPost "/taps" ["where" .= object ["venueId" .= venueId], "limit" .= limit, "order" .= ("-createdAt" :: Text)]
  pure $ r ^.. key "results" . _Array . traverse
    . to (\o -> Beer
        (o ^?! key "beerName" . _String)
        (o ^?! key "breweryName" . _String)
        (o ^?! key "style" . _String)
        (o ^?! key "abv" . _Double)
      )

ntPost :: Text -> [(Text, Value)] -> EitherT Text IO Value
ntPost path parameters =
  let
    prop = [
      "_method" .= ("GET" :: Text)
      -- Taken from the publicly exposed keys in:
      --   https://nowtapped.com/js/nowtapped.js
      , "_ApplicationId" .= ("1U4t4M6xOdc8fh8XbZgdZTGUUruR7zwObFq8Cffd" :: Text)
      , "_JavaScriptKey" .= ("RVKbXblBWqfnpEikhWVlvzL9xXBJJbdRqGxnMdtk" :: Text)
      , "_ClientVersion" .= ("js1.2.18" :: Text)
      , "_InstallationId" .= ("8fafd32a-5f93-7d42-f8a7-a437de0a5179" :: Text)
      ] <> parameters
  in
    E.handle (\(e :: HttpException) -> left . T.pack $ show e) $
      lift . fmap (view responseBody) . (=<<) asValue $
      post (T.unpack $ "https://nowtapped-1401.nodechef.com/parse/classes" <> path) . encode . object $
      prop

{-# LANGUAGE OverloadedStrings #-}

module Utils.Spoty where

import           Control.Lens
import           Data.Aeson
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wreq as W
import           Utils.Spoty.Types

baseURL, versionURL :: String
baseURL    = "https://api.spotify.com/"
versionURL = "v1/"

-- perhaps merge consecutive queries in some clever way?

-- | TBD.
getAlbum :: SpotID -> IO Album
getAlbum = fetch "albums"

-- | TBD.
getArtist :: SpotID -> IO Artist
getArtist = fetch "artists"

{-
-- | TBD.
search :: _ -> IO _
search = undefined
-}

-- | TBD.
getTrack :: SpotID -> IO Track
getTrack = fetch "tracks"

-- | TBD.
getUser :: T.Text -> IO User
getUser = fetch "users"

-- | TBD.
fetch :: FromJSON a => String -> T.Text -> IO a
fetch endpoint arg = do
  reply <- W.get $ baseURL <> versionURL <> endpoint <> "/" <> T.unpack arg
  fmap (^. W.responseBody) (W.asJSON reply)

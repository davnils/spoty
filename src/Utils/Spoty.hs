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

build :: T.Text -> T.Text -> T.Text
build e d = build2 e "" d

build2 :: T.Text -> T.Text -> T.Text -> T.Text
build2 endpoint dir arg = T.intercalate "/" [endpoint, arg, dir]

-- | TBD.
getAlbum :: SpotID -> IO Album
getAlbum = fetch . build "albums"

-- | TBD.
getArtist :: SpotID -> IO Artist
getArtist = fetch . build "artists"

-- | TBD.
getArtistAlbums :: SpotID -> IO (Paging Album)
getArtistAlbums = fetch . build2 "artists" "albums"

{-
-- | TBD.
search :: _ -> IO _
search = undefined
-}

-- | TBD.
getTrack :: SpotID -> IO Track
getTrack = fetch . build "tracks"

-- | TBD.
getUser :: T.Text -> IO User
getUser = fetch . build "users"

-- | TBD.
fetch :: FromJSON a => T.Text -> IO a
fetch endpoint = do
  reply <- W.get $ baseURL <> versionURL <> T.unpack endpoint
  fmap (^. W.responseBody) (W.asJSON reply)

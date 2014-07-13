{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Utils.Spoty
(
getAlbum, getAlbumTracks,
getArtist, getArtistAlbums, CountryID, getArtistTop, getArtistRelated,
SearchCategory(..),
search, searchArtist, searchAlbum, searchTrack,
getTrack,
getUser,
fetchOne, fetchAll
)
where

import           Control.Applicative ((<$>))
import           Control.Exception (throw)
import           Control.Lens
import           Control.Monad (when)
import           Data.Aeson
import           Data.Aeson.Lens (key)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Pipes as P
import qualified Pipes.Prelude as P
import           Utils.Spoty.Types

-- | TBD.
baseURL, versionURL :: String
baseURL    = "https://api.spotify.com/"
versionURL = "v1/"

-- | TBD.
type CountryID = T.Text

-- | TBD.
data SearchCategory
  = SearchAlbum  -- ^ TBD.
  | SearchArtist -- ^ TBD.
  | SearchTrack  -- ^ TBD.
  deriving (Eq, Ord)

instance Show SearchCategory where
  show SearchAlbum = "album"
  show SearchArtist = "artist"
  show SearchTrack = "track"

-- | TBD.
build :: T.Text -> T.Text -> T.Text
build e = build2 e ""

-- | TBD.
build2 :: T.Text -> T.Text -> T.Text -> T.Text
build2 endpoint dir arg = T.intercalate "/" [endpoint, arg, dir]

-- | TBD.
getAlbum :: SpotID -> IO Album
getAlbum = fetch . build "albums"

-- | TBD.
getAlbumTracks :: SpotID -> P.Producer Track IO ()
getAlbumTracks = makeProducer Nothing W.defaults . build2 "albums" "tracks"

-- | TBD.
getArtist :: SpotID -> IO Artist
getArtist = fetch . build "artists"

-- | TBD.
getArtistAlbums :: SpotID -> P.Producer Album IO ()
getArtistAlbums = makeProducer Nothing W.defaults . build2 "artists" "albums"

type Predicate a = W.Response BL.ByteString -> IO a

-- | Construct producer (source) from URL generating a paging object.
--   Optionally accepts a predicate applied on the retrieved JSON object.
makeProducer :: FromJSON a => Maybe (Predicate (Paging a)) -> W.Options -> T.Text -> P.Producer a IO ()
makeProducer predicate opts url = go 0
  where
  go off = do
    let opts' = opts & W.param "offset" .~ [T.pack $ show off]
    reply <- P.liftIO $ grab opts' url

    let f = fromMaybe (fmap (^. W.responseBody) . W.asJSON) predicate
    (chunk :: Paging b) <- P.liftIO $ f reply

    mapM_ P.yield $ chunk ^. items

    let delta = length $ chunk ^. items
        off' = off + delta

    when (off' < chunk ^. total) (go off')

-- | Extract the value associated with the given key from a response.
extractInner :: (FromJSON a) => W.Response BL.ByteString -> T.Text -> Maybe a
extractInner raw tag = locate >>= parseMaybe parseJSON
  where
  locate = raw ^? W.responseBody . key tag

-- | TBD.
getArtistTop :: SpotID -> CountryID -> IO [Track]
getArtistTop artist country = do
  let opts = W.defaults & W.param "country" .~ [country]
  reply <- grab opts $ build2 "artists" "top-tracks" artist
  return . fromMaybe [] $ extractInner reply "tracks"

-- | TBD.
getArtistRelated :: SpotID -> IO [Artist]
getArtistRelated artist = do
  reply <- grab W.defaults $ build2 "artists" "related-artists" artist
  return . fromMaybe [] $ extractInner reply "artists"

-- | TBD.
getTrack :: SpotID -> IO Track
getTrack = fetch . build "tracks"

-- | TBD.
getUser :: T.Text -> IO User
getUser = fetch . build "users"

-- | TBD.
search :: [SearchCategory] -> T.Text -> (P.Producer Artist IO (), P.Producer Album IO (), P.Producer Track IO ())
search cats term = (extract SearchArtist, extract SearchAlbum, extract SearchTrack)
  where
  opts = W.defaults
         & W.param "q" .~ [term]
         & W.param "type" .~ [T.intercalate "," $ map (T.pack . show) cats]

  pluralize = T.pack . (<> "s") . show
  extract tag = when (tag `elem` cats) (makeProducer (Just $ predicate tag) opts url)
  url = build "search" ""
  predicate tag reply = case extractInner reply (pluralize tag) of
    Just val -> return val
    Nothing  -> throw . W.JSONError $ "Unexpected search result, got: " <> show reply

-- | TBD.
searchArtist :: T.Text -> P.Producer Artist IO ()
searchArtist = (^. _1) . search [SearchArtist]

-- | TBD.
searchAlbum :: T.Text -> P.Producer Album IO ()
searchAlbum = (^. _2) . search [SearchAlbum]

-- | TBD.
searchTrack :: T.Text -> P.Producer Track IO ()
searchTrack = (^. _3) . search [SearchTrack]

-- | TBD.
fetchOne :: Monad m => P.Producer a m () -> m (Maybe a)
fetchOne = P.head

-- | TBD.
fetchAll :: Monad m => P.Producer a m () -> m [a]
fetchAll = P.toListM

-- | TBD.
fetch :: FromJSON a => T.Text -> IO a
fetch = fetchWith W.defaults

-- | TBD.
fetchWith :: FromJSON a => W.Options -> T.Text -> IO a
fetchWith opts endpoint =
  (^. W.responseBody) <$> (grab opts endpoint >>= W.asJSON)

-- | TBD.
grab :: W.Options -> T.Text -> IO (W.Response BL.ByteString)
grab opts endpoint =
  W.getWith opts $ baseURL <> versionURL <> T.unpack endpoint

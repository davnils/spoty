{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Utils.Spoty where

import           Control.Applicative ((<$>))
import           Control.Exception (throw)
import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
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

baseURL, versionURL :: String
baseURL    = "https://api.spotify.com/"
versionURL = "v1/"

type CountryID = T.Text

data SearchCategory
  = SearchAlbum
  | SearchArtist
  | SearchTrack
  deriving (Eq, Ord)

instance Show SearchCategory where
  show SearchAlbum = "album"
  show SearchArtist = "artist"
  show SearchTrack = "track"

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
getArtistAlbums :: SpotID -> P.Producer Album IO ()
getArtistAlbums = makeProducer Nothing W.defaults . build2 "artists" "albums"

type Predicate a = W.Response BL.ByteString -> IO a

-- | Construct producer (source) from URL generating a paging object.
--   Optionally accepts a predicate applied on the retrieved JSON object.
makeProducer :: FromJSON a => Maybe (Predicate (Paging a)) -> W.Options -> T.Text -> P.Producer a IO ()
makeProducer pred opts url = go 0
  where
  go offset = do
    let opts' = opts & W.param "offset" .~ [T.pack $ show offset]
    reply <- liftIO $ grab opts' url

    let f = fromMaybe (fmap (^. W.responseBody) . W.asJSON) pred
    (chunk :: Paging b) <- liftIO $ f reply

    -- TODO: REMOVE
    -- liftIO . putStrLn $ "(offset, total)=" <> show (offset, chunk ^. total)

    mapM_ P.yield $ chunk ^. items

    let delta = length $ chunk ^. items
        offset' = offset + delta

    when (offset' < chunk ^. total) (go offset')

-- | Extract the value associated with the given key from a response.
extractInner :: (FromJSON a) => W.Response BL.ByteString -> T.Text -> Maybe a
extractInner raw tag = lookup >>= parseMaybe parseJSON
  where
  lookup = raw ^? W.responseBody . key tag

-- | TBD.
getArtistTop :: SpotID -> CountryID -> IO ([Track])
getArtistTop artist country = do
  let opts = W.defaults & W.param "country" .~ [country]
  reply <- grab opts $ build2 "artists" "top-tracks" artist
  return . fromMaybe [] $ extractInner reply "tracks"

-- | TBD.
getArtistRelated :: SpotID -> IO ([Artist])
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
  extract tag = when (elem tag cats) (makeProducer (Just $ pred tag) opts url)
  url = build "search" ""
  pred tag reply = case extractInner reply (pluralize tag) of
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

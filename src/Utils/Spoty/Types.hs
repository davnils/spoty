{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}

{-|
   Object declarations and lenses. Should not be imported by user code.
   Please view the official documentation.

   Note that the distinction between full and simple objects is implemented as an optional Maybe field with details.
-}

module Utils.Spoty.Types where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens (makeFields)
import           Control.Monad (MonadPlus(..), mzero)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type URL = T.Text

type SpotID = T.Text

type SpotURI = T.Text

-- | Require that a field is present before parsing the corresponding value.
require :: FromJSON a => T.Text -> HM.HashMap T.Text Value -> Parser (Maybe a)
require str obj =
  if HM.member str obj
    then fmap Just (parseJSON $ Object obj)
    else return Nothing

-- | Parse a map of key-value entries, wrapped in the given constructor.
parseStrMap :: MonadPlus m => HM.HashMap k Value -> (k -> T.Text -> a) -> m [a]
parseStrMap vals constr = sequence . flip map (HM.toList vals) $ \e ->
  case e of 
    (key, String val) -> return $ constr key val
    _                 -> mzero

data ExternalID
  = ExternalID
  {
    _idType :: T.Text,
    _idIdentifier :: T.Text
  }
  deriving (Eq, Ord, Show)

makeFields ''ExternalID

instance FromJSON [ExternalID] where
  parseJSON (Object v) = parseStrMap v ExternalID
  parseJSON _          = mzero

data ExternalURL
  = ExternalURL
  {
    _urlType :: T.Text,
    _url :: URL
  }
  deriving (Eq, Ord, Show)

makeFields ''ExternalURL

instance FromJSON [ExternalURL] where
  parseJSON (Object v) = parseStrMap v ExternalURL
  parseJSON _          = mzero

data Image
  = Image
  {
    _imageHeight :: Maybe Int,
    _imagePath :: URL,
    _imageWidth :: Maybe Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Image

instance FromJSON Image where
  parseJSON (Object v) = Image <$>
                         v .:? "height" <*>
                         v .: "url" <*>
                         v .:? "width"

  parseJSON _          = mzero

data Paging a
  = Paging
  {
    _pagingHref :: T.Text,
    _pagingItems :: [a],
    _pagingLimit :: Int,
    _pagingNext :: Maybe URL,
    _pagingOffset :: Int,
    _pagingPrevious :: Maybe URL,
    _pagingTotal :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''Paging

instance FromJSON a => FromJSON (Paging a) where
  parseJSON (Object v) = Paging <$>
                         v .: "href" <*>
                         v .: "items" <*>
                         v .: "limit" <*>
                         v .:? "next" <*>
                         v .: "offset" <*>
                         v .:? "previous" <*>
                         v .: "total"

  parseJSON _          = mzero

data User
  = User
  {
    _userExternalUrls :: [ExternalURL],
    _userHref :: URL,
    _userSpotifyID :: SpotID,
    _userSpotifyURI :: SpotURI
  }
  deriving (Eq, Ord, Show)

makeFields ''User

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "external_urls" <*>
                         v .: "href" <*>
                         v .: "id" <*>
                         v .: "uri"

  parseJSON _          = mzero

data ArtistDetails
  = ArtistDetails
  {
    _artistGenres :: [T.Text],
    _artistImages :: [Image],
    _artistPopularity :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''ArtistDetails

instance FromJSON ArtistDetails where
  parseJSON (Object v) = ArtistDetails <$>
                         v .: "genres" <*>
                         v .: "images" <*>
                         v .: "popularity"

  parseJSON _          = mzero

data Artist
  = Artist
  {
    _artistExternalUrls :: [ExternalURL],
    _artistHref :: URL,
    _artistSpotifyID :: SpotID,
    _artistName :: T.Text,
    _artistSpotifyURI :: SpotURI,
    _artistDetails :: Maybe ArtistDetails
  }
  deriving (Eq, Ord, Show)

makeFields ''Artist

instance FromJSON Artist where
  parseJSON (Object v) = Artist <$>
                         v .: "external_urls" <*>
                         v .: "href" <*>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "uri" <*> 
                         require "genres" v

  parseJSON _          = mzero

data TrackDetails
  = TrackDetails
  {
    _trackAvailableMarkets :: [T.Text],
    _trackExternalIDs :: [ExternalID],
    _trackPopularity :: Int
  }
  deriving (Eq, Ord, Show)

makeFields ''TrackDetails

instance FromJSON TrackDetails where
  parseJSON (Object v) = TrackDetails <$>
                         v .: "available_markets" <*>
                         v .: "external_ids" <*>
                         v .: "popularity"

  parseJSON _          = mzero

data Track
  = Track
  {
    _trackArtists :: [Artist],
    _trackDiscNumber :: Int,
    _trackDurationMs :: Int,
    _trackExplicit :: Bool,
    _trackExternalUrls :: [ExternalURL],
    _trackHref :: URL,
    _trackSpotifyID :: SpotID,
    _trackName :: T.Text,
    _trackPreviewURL :: URL,
    _trackNumber :: Int,
    _trackSpotifyURI :: SpotURI,
    _trackDetails :: Maybe TrackDetails
  }
  deriving (Eq, Ord, Show)

makeFields ''Track

instance FromJSON Track where
  parseJSON (Object v) = Track <$>
                         v .: "artists" <*>
                         v .: "disc_number" <*>
                         v .: "duration_ms" <*>
                         v .: "explicit" <*>
                         v .: "external_urls" <*>
                         v .: "href" <*>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "preview_url" <*>
                         v .: "track_number" <*>
                         v .: "uri" <*> 
                         require "available_markets" v

  parseJSON _          = mzero

data AlbumDetails
  = AlbumDetails
  {
    _albumArtists :: [Artist],
    _albumExternalIDs :: [ExternalID],
    _albumGenres :: [T.Text],
    _albumPopularity :: Int,
    _albumReleaseDate :: T.Text,
    _albumReleaseDatePrecision :: T.Text,
    _albumTracks :: Paging Track
  }
  deriving (Eq, Ord, Show)

makeFields ''AlbumDetails

instance FromJSON AlbumDetails where
  parseJSON (Object v) = AlbumDetails <$>
                         v .: "artists" <*>
                         v .: "external_ids" <*>
                         v .: "genres" <*>
                         v .: "popularity" <*>
                         v .: "release_date" <*>
                         v .: "release_date_precision" <*>
                         v .: "tracks"

  parseJSON _          = mzero

data Album
  = Album
  {
    _albumType :: T.Text,
    _albumAvailableMarkets :: [T.Text],
    _albumExternalURLs :: [ExternalURL],
    _albumHref :: T.Text,
    _albumSpotifyID :: SpotID,
    _albumImages :: [Image],
    _albumName :: T.Text,
    _albumSpotifyURI :: SpotURI,
    _albumDetails :: Maybe AlbumDetails
  }
  deriving (Eq, Ord, Show)

makeFields ''Album

instance FromJSON Album where
  parseJSON (Object v) = Album <$>
                         v .: "album_type" <*>
                         v .: "available_markets" <*>
                         v .: "external_urls" <*>
                         v .: "href" <*>
                         v .: "id" <*>
                         v .: "images" <*>
                         v .: "name" <*>
                         v .: "uri" <*>
                         require "artists" v

  parseJSON _          = mzero

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Utils.Spoty.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type URL = T.Text
type SpotID = T.Text
type SpotURI = T.Text

require str obj = case HM.member str obj of
                    False -> return Nothing
                    True -> fmap Just (parseJSON $ Object obj)

-- TODO: ExternalID and ExternalURL may need to accept an array

data ExternalID
  = ExternalID
  {
    _idType :: T.Text,
    _idIdentifier :: T.Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''ExternalID

instance FromJSON ExternalID where
  parseJSON (Object v) = case (HM.toList v) of
                           [(key, String val)] -> return $ ExternalID key val
                           _            -> mzero
  parseJSON _          = mzero

data ExternalURL
  = ExternalURL
  {
    _urlType :: T.Text,
    _url :: URL
  }
  deriving (Eq, Ord, Show)

makeLenses ''ExternalURL

instance FromJSON ExternalURL where
  parseJSON (Object v) = case (HM.toList v) of
                           [(key, String val)] -> return $ ExternalURL key val
                           _            -> mzero
  parseJSON _          = mzero

data Image
  = Image
  {
    _imageHeight :: Maybe Int,
    _imageURL :: URL,
    _imageWidth :: Maybe Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''Image

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

makeLenses ''Paging

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
    _userExternalUrls :: ExternalURL,
    _userHref :: URL,
    _userID :: SpotID,
    _userURI :: SpotURI
  }
  deriving (Eq, Ord, Show)

makeLenses ''User

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

makeLenses ''ArtistDetails

instance FromJSON ArtistDetails where
  parseJSON (Object v) = ArtistDetails <$>
                         v .: "genres" <*>
                         v .: "images" <*>
                         v .: "popularity"

  parseJSON _          = mzero

data Artist
  = Artist
  {
    _artistExternalUrls :: ExternalURL,
    _artistHref :: URL,
    _artistID :: SpotID,
    _artistName :: T.Text,
    _artistURI :: SpotURI,
    _artistDetails :: Maybe ArtistDetails
  }
  deriving (Eq, Ord, Show)

makeLenses ''Artist

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
    _trackExternalIDs :: ExternalID,
    _trackPopularity :: Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''TrackDetails

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
    _trackExternalUrls :: ExternalURL,
    _trackHref :: URL,
    _trackID :: SpotID,
    _trackName :: T.Text,
    _trackPreviewURL :: URL,
    _trackNumber :: Int,
    _trackURI :: SpotURI,
    _trackDetails :: Maybe TrackDetails
  }
  deriving (Eq, Ord, Show)

makeLenses ''Track

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
    _albumAvailableMarkets :: [T.Text],
    _albumExternalIDs :: ExternalID,
    _albumGenres :: [T.Text],
    _albumPopularity :: Int,
    _albumReleaseDate :: T.Text,
    _albumReleaseDatePrecision :: T.Text,
    _albumTracks :: Paging Track
  }
  deriving (Eq, Ord, Show)

makeLenses ''AlbumDetails

instance FromJSON AlbumDetails where
  parseJSON (Object v) = AlbumDetails <$>
                         v .: "artists" <*>
                         v .: "available_markets" <*>
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
    _albumExternalURLs :: ExternalURL,
    _albumHref :: T.Text,
    _albumID :: SpotID,
    _albumImages :: [Image],
    _albumName :: T.Text,
    _albumURI :: SpotURI,
    _albumDetails :: Maybe AlbumDetails
  }
  deriving (Eq, Ord, Show)

makeLenses ''Album

instance FromJSON Album where
  parseJSON (Object v) = Album <$>
                         v .: "album_type" <*>
                         v .: "external_urls" <*>
                         v .: "href" <*>
                         v .: "id" <*>
                         v .: "images" <*>
                         v .: "name" <*>
                         v .: "uri" <*>
                         require "artists" v

  parseJSON _          = mzero

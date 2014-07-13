{-# LANGUAGE OverloadedStrings  #-}

import           Control.Lens (view)
import qualified Data.Text.IO as T
import           Utils.Spoty
import           Utils.Spoty.Types

main = do
  Just artist <- fetchOne $ searchArtist "avicii"      -- assume at least one match
  popular <- getArtistTop (view spotifyID artist) "SE" -- separate API call on the artist ID
  mapM_ (T.putStrLn . view name) popular               -- inspect the returned tracks

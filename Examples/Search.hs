{-# LANGUAGE OverloadedStrings  #-}

import           Control.Lens (view)
import           Utils.Spoty

main = do
  Just artist <- fetchOne (searchArtist "avicii")      -- assume at least one match
  popular <- getArtistTop (view spotifyID artist) "SE" -- retrieve the most popular tracks in Sweden
  mapM_ (print . view name) popular                    -- print the corresponding names

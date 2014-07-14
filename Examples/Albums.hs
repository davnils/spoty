{-# LANGUAGE OverloadedStrings  #-}

import           Control.Lens
import           Pipes
import qualified Pipes.Prelude as P
import           Utils.Spoty

-- | Stream all albums in constant space and print the names.
main = runEffect $ getArtistAlbums artist >-> P.map (view name) >-> P.print
  where
  artist = "0LcJLqbBmaGUft1e9Mm8HV"

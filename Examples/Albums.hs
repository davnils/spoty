{-# LANGUAGE OverloadedStrings  #-}

import           Control.Lens
import           Pipes
import qualified Pipes.Prelude as P
import           Utils.Spoty

main = runEffect $ getArtistAlbums artist >-> P.map (^. albumName) >-> P.print
  where
  artist = "0LcJLqbBmaGUft1e9Mm8HV"

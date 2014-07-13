# Spotify web API in Haskell

Web API wrapper powered by lens and pipes, allowing easy access to public endpoints.
It does not have any external dependencies nor requirements regarding app registration.

* Paging is handled transparently using pipes
* All data types are navigated using lenses

All public endpoints, with multi-get versions excluded, are implemented.

## Example
    > :set -XOverloadedStrings
    > import           Control.Lens (view)
    > import qualified Data.Text.IO as T
    > import           Utils.Spoty
    > import           Utils.Spoty.Types

    > Just artist <- fetchOne $ searchArtist "avicii"      -- assume at least one match
    > popular <- getArtistTop (view spotifyID artist) "SE" -- separate API call on the artist ID
    > mapM_ (T.putStrLn . view name) popular               -- inspect the returned tracks

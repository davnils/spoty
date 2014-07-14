# Spotify web API in Haskell

Web API wrapper powered by lens and pipes, allowing easy access to public endpoints.
It does not have any external dependencies nor requirements regarding app registration.

* Paging is handled transparently using pipes
* All data types are navigated using lenses

All public endpoints, with multi-get versions excluded, have been implemented.

## Example (Examples/Search.hs)
    > :set -XOverloadedStrings
    > :m +Control.Lens Utils.Spoty
    > Just artist <- fetchOne (searchArtist "avicii")      -- assume at least one match
    > popular <- getArtistTop (view spotifyID artist) "SE" -- retrieve the most popular tracks in Sweden
    > mapM_ (print . view name) popular                    -- print the corresponding names
    "Hey Brother"
    "Addicted To You"
    "Wake Me Up"
    ...

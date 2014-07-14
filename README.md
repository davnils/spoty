# Spotify web API in Haskell

[Web API](https://developer.spotify.com/web-api/) wrapper powered by lens and pipes, allowing easy access to public endpoints.
It does not have any external dependencies nor requirements regarding app registration.

* Paging is handled transparently using pipes
* All data types are navigated using lenses

All public endpoints, with multi-get versions excluded, are implemented.

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

## Design
The *Utils.Spoty* module provides access to all of the included endpoints, as listed in the [official documentation](https://developer.spotify.com/web-api/endpoint-reference/).
All of the returned objects are defined in *Utils.Spoty.Types* and have associated lenses.
The names have been preserved to a large extent, but URIs and IDs are prefixed with *spotify*.

Some of the API endpoints return a (potentially long) stream of objects, e.g. when performing a search.
This is handled by using *pipes*. Consider the following signature:

    searchArtist ⩬ T.Text → P.Producer Artist IO ∅

Here the returned value is a producer of artist objects.
If you don't need constant-space streaming, then you can extract the corresponding list by calling `fetchAll ⩬ Monad m ⇒ P.Producer a m ∅ → m [a]`.
When you're only interested in the first result (e.g. when searching for a well-known artist), it's useful to call `fetchOne ⩬ Monad m ⇒ P.Producer a m ∅ → m (Maybe a)`.
In the other cases you will need functions available in the pipes package.

Finally, error handling is implemented with exceptions being thrown when HTTP or JSON decoding errors occur.

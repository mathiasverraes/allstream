module Projection where

import           Database.HDBC.PostgreSQL (Connection)
import           Domain.Events            (DomainEvent)

data Projection a b =
    Projection
        { initState :: a
        , step      :: a -> DomainEvent -> a
        , query     :: a -> b
        }
{-
replay :: EventStore -> Projection a b -> IO b
replay eventStore projection = do
    events <- stream eventStore
    let state = foldl' (projection |> step) (projection |> initState) events
    return $ (projection |> query) state
-}

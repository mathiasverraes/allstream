module Projection where

import           Database.HDBC.PostgreSQL (Connection)
import           Domain.Events            (DomainEvent)
import           EventStore
import           Flow
import Data.List (foldl')

data Projection state response =
    Projection
        { initState :: state
        , step      :: state -> DomainEvent -> state
        , query     :: state -> response
        }

replay :: Stream -> Projection a b -> IO b
replay stream projection = do
    let state = foldl' (projection |> step) (projection |> initState) (eventPayload <$> stream)
    return $ (projection |> query) state

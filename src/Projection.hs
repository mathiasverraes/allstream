module Projection where

import           EventStore
import           Flow
import Data.List (foldl')

data Projection event state response =
    Projection
        { initState :: state
        , step      :: state -> event -> state
        , query     :: state -> response
        }

replay :: Stream event -> Projection event state response -> IO response
replay stream projection = do
    let state = foldl' (projection |> step) (projection |> initState) (eventPayload <$> stream)
    return $ (projection |> query) state

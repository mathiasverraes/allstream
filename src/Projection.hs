module Projection where

import           EventStore
import           Flow
import Data.List (foldl')

data Projection event state response =
    Projection
        { initState :: state
        -- The reason for not doing state -> event -> state, is that it makes for more readable 
        -- projections. eg when event state = state
        , step      :: event -> state -> state
        , query     :: state -> response
        }

replay :: Stream event -> Projection event state response -> IO response
replay stream projection = do
    let state = foldl' (projection |> (flip . step)) (projection |> initState) (eventPayload <$> stream)
    return $ (projection |> query) state

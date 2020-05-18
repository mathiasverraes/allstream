{-# LANGUAGE NamedFieldPuns #-}

module Projection where

import           Data.List  (foldl')
import           EventStore
import           Flow

data Projection event state response =
    Projection
        { initState :: state
        -- The reason for not doing state -> event -> state, is that it makes for more readable
        -- projections. eg when event state = state
        , step      :: event -> state -> state
        , transform :: state -> response
        }

replay :: Stream event -> Projection event state response -> response
replay stream Projection {initState, step, transform} =
    foldl' (flip step) initState (eventPayload <$> stream) |> transform

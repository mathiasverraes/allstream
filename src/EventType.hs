module EventType
    ( EventType
    , eventType
    ) where

class EventType a where
    eventType :: a -> String

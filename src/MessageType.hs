module MessageType where

import qualified Data.HashMap.Strict as H

-- * Events
type EventTypeName = String

data EventType =
    EventType
        { etType    :: EventTypeName
        , etAttribs :: H.HashMap AttribName AttribType
        }

type EventTypes = H.HashMap EventTypeName EventType

-- * Commands
type CommandTypeName = String

data CommandType =
    CommandType
        { ctType    :: CommandTypeName
        , ctAttribs :: H.HashMap AttribName AttribType
        }

type CommandTypes = H.HashMap CommandTypeName CommandType

-- * Attribs

data AttribName =
    AttribName
    deriving (Show, Eq)

data AttribType =
    AttribType
    deriving (Show, Eq)

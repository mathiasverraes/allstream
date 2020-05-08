{-# LANGUAGE DuplicateRecordFields #-}

module EventStore where

import           Data.Aeson               (ToJSON, Value, decode, encode)
import           Data.Map.Strict          ((!))
import           Data.Maybe               (fromJust)
import qualified Data.UUID                as UUID (UUID, fromByteString,
                                                   fromString, toString)
import qualified Data.UUID.V4             as UUID (nextRandom)
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Domain.Commands
import           Domain.Events
import qualified EventType                (eventType)
import qualified Data.ByteString          as BS (unpack)

type EventType = String

type EventId = UUID.UUID

data PersistedEvent =
    Persisted
        { partitionSequenceNr :: Int
        , eventId             :: EventId
        , eventType           :: EventType
        , eventPayload        :: DomainEvent
        , recordedTime        :: String
        }
    deriving (Show, Eq)
type Stream = [PersistedEvent]

connect :: IO Connection
connect = connectPostgreSQL "host=localhost dbname=allstreamtest user="

stream :: Connection -> IO Stream
stream conn = do
    stmt <- prepare conn "SELECT * FROM events ORDER BY partition_sequence_nr ASC;"
    --"SELECT partition_sequence_nr, event_id, event_type, domain_event_type, event_payload, recorded_time FROM events ORDER BY partition_sequence_nr ASC;"
    execute stmt []
    rows <- fetchAllRowsMap stmt
    return $ toPersisted <$> rows
  where
    toPersisted row =
        Persisted
            { partitionSequenceNr = fromSql $ row ! "partition_sequence_nr"
            , eventId = fromJust $ UUID.fromString $ fromSql (row ! "event_id")
            , eventType = fromSql $ row ! "event_type"
            , eventPayload = fromJust $ decode $ fromSql $ row ! "event_payload"
            , recordedTime = fromSql $ row ! "recorded_time"
            }

append :: Connection -> DomainEvent -> IO ()
append conn domainEvent = do
    eventId <- UUID.nextRandom
    stmt <- prepare conn "CALL append(?, ?, ?);"
    execute
        stmt
        [ toSql $ UUID.toString eventId
        , toSql $ EventType.eventType domainEvent
        , toSql $ encode domainEvent
        ]
    commit conn

{-# LANGUAGE DuplicateRecordFields #-}

module EventStore where

import           Data.Aeson               (FromJSON, ToJSON, Value, decode,
                                           encode)
import qualified Data.ByteString          as BS (unpack)
import           Data.Map.Strict          ((!))
import           Data.Maybe               (fromJust)
import qualified Data.UUID                as UUID (UUID, fromByteString,
                                                   fromString, toString)
import qualified Data.UUID.V4             as UUID (nextRandom)
import           Database.HDBC
import           Database.HDBC.PostgreSQL

type EventType = String

type EventId = UUID.UUID

data PersistedEvent e =
    Persisted
        { partitionSequenceNr :: Int
        , eventType           :: EventType
        , eventId             :: EventId
        , eventPayload        :: e
        , recordedTime        :: String
        }
    deriving (Show, Eq)

type Stream e = [PersistedEvent e]

type StreamType = String

type StreamId = UUID.UUID

class (FromJSON a, ToJSON a) =>
      IsDomainEvent a
    where
    eventType :: a -> EventType

data Aggregate =
    Aggregate
        { name            :: String
        , supportedEvents :: [EventType]
        }

connect :: IO Connection
connect = connectPostgreSQL "host=localhost dbname=allstreamtest user="

stream :: (IsDomainEvent e) => Connection -> IO (Stream e)
stream conn = do
    stmt <-
        prepare
            conn
            "SELECT partition_sequence_nr, event_type, event_id, event_payload, recorded_time  FROM events ORDER BY partition_sequence_nr ASC;"
    execute stmt []
    rows <- fetchAllRowsMap stmt
    return $ toPersisted <$> rows
  where
    toPersisted row =
        Persisted
            { partitionSequenceNr = fromSql $ row ! "partition_sequence_nr"
            , eventType = fromSql $ row ! "event_type"
            , eventId = fromJust $ UUID.fromString $ fromSql (row ! "event_id")
            , eventPayload = fromJust $ decode $ fromSql $ row ! "event_payload"
            , recordedTime = fromSql $ row ! "recorded_time"
            }

append :: (IsDomainEvent event) => Connection -> event -> IO ()
append conn event = do
    eventId <- UUID.nextRandom
    stmt <- prepare conn "CALL append(?, ?, ?);"
    execute stmt [toSql $ eventType event, toSql $ UUID.toString eventId, toSql $ encode event]
    commit conn

appendToStream ::
       (IsDomainEvent event) => Connection -> StreamType -> StreamId -> Int -> event -> IO ()
appendToStream conn streamType streamId expectedStreamSequenceNr event = do
    eventId <- UUID.nextRandom
    stmt <- prepare conn "CALL append_to_stream(?, ?, ?, ?, ?, ?);"
    execute
        stmt
        [ toSql streamType
        , toSql $ UUID.toString streamId
        , toSql expectedStreamSequenceNr
        , toSql $ eventType event
        , toSql $ UUID.toString eventId
        , toSql $ encode event
        ]
    commit conn

registerStreamType :: Connection -> StreamType -> IO ()
registerStreamType conn streamType = do
    stmt <- prepare conn "CALL register_stream_type(?);"
    execute stmt [toSql streamType]
    commit conn
    return ()

startNewStream :: Connection -> StreamType -> StreamId -> IO ()
startNewStream conn streamType streamId = do
    stmt <- prepare conn "CALL start_new_stream(?, ?);"
    execute stmt [toSql streamType, toSql $ UUID.toString streamId]
    commit conn
    return ()

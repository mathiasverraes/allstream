{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module EventStore
    ( EventStore
    , Payload(eventType)
    , PersistedEvent(..)
    , Stream
    , StreamType
    , StreamId
    , connect
    , fetchAll
    , fetchStream
    , appendToStream
    ) where

import           Data.Aeson               (FromJSON, ToJSON, decode, encode)
import           Data.Map.Strict          (Map, (!))
import           Data.Maybe               (fromJust)
import qualified Data.UUID                as UUID (UUID, fromString, toString)
import qualified Data.UUID.V4             as UUID (nextRandom)
import           Database.HDBC
import           Database.HDBC.PostgreSQL

type EventStore e = Connection

type EventType = String

type EventId = UUID.UUID

type Stream e = [PersistedEvent e]

type StreamType = String

type StreamId = UUID.UUID

data PersistedEvent e =
    Persisted
        { partitionSeq :: Int
        , eventType    :: EventType
        , eventId      :: EventId
        , eventPayload :: e
        , recordedTime :: String
        , streamType   :: StreamType
        , streamId     :: StreamId
        , streamSeq    :: Int
        }

class (FromJSON a, ToJSON a, Show a) =>
      Payload a
    where
    eventType :: a -> EventType

connect :: IO (EventStore e)
connect = connectPostgreSQL "host=localhost dbname=allstreamtest user="

fetchAll :: (Payload e) => EventStore e -> IO (Stream e)
fetchAll conn = do
    stmt <-
        prepare
            conn
            "SELECT partition_seq, event_type, event_id, event_payload, recorded_time, stream_type, stream_id, stream_seq FROM events e ORDER BY partition_seq ASC"
    execute stmt []
    rows <- fetchAllRowsMap stmt
    return $ toPersisted <$> rows

fetchStream :: (Payload e) => EventStore e -> StreamType -> StreamId -> IO (Stream e)
fetchStream conn streamType streamId = do
    stmt <-
        prepare
            conn
            "SELECT partition_seq, event_type, event_id, event_payload, recorded_time, stream_type, stream_id, stream_seq FROM events e WHERE stream_type = ? AND stream_id = ? ORDER BY stream_seq ASC"
    execute stmt [toSql streamType, toSql $ UUID.toString streamId]
    rows <- fetchAllRowsMap stmt
    return $ toPersisted <$> rows

toPersisted :: Payload e => Map String SqlValue -> PersistedEvent e
toPersisted row =
    Persisted
        { partitionSeq = fromSql $ row ! "partition_seq"
        , eventType = fromSql $ row ! "event_type"
        , eventId = fromJust $ UUID.fromString $ fromSql (row ! "event_id")
        , eventPayload = fromJust $ decode $ fromSql $ row ! "event_payload"
        , recordedTime = fromSql $ row ! "recorded_time"
        , streamType = fromSql $ row ! "stream_type"
        , streamId = fromJust $ UUID.fromString $ fromSql (row ! "stream_id")
        , streamSeq = fromSql $ row ! "stream_seq"
        }

appendToStream :: (Payload event) => EventStore e -> StreamType -> StreamId -> Int -> event -> IO ()
appendToStream conn streamType streamId expectedStreamSeq event = do
    eventId <- UUID.nextRandom
    stmt <- prepare conn "CALL append_to_stream(?, ?, ?, ?, ?, ?);"
    execute
        stmt
        [ toSql $ eventType event
        , toSql $ UUID.toString eventId
        , toSql $ encode event
        , toSql streamType
        , toSql $ UUID.toString streamId
        , toSql expectedStreamSeq
        ]
    commit conn

instance (Payload e) => Show (PersistedEvent e) where
    show Persisted {partitionSeq, eventPayload, streamType, streamId, streamSeq, recordedTime} =
        "#" ++
        show partitionSeq ++
        "\t" ++
        streamType ++
        " v" ++
        show streamSeq ++
        " (" ++
        take 7 (show streamId) ++
        ")\t" ++
        take 19 recordedTime ++
        "\n\t" ++ show eventPayload ++ "\n"

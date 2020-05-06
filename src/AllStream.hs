module AllStream
    ( go
    ) where

import qualified Data.UUID                as UUID (UUID, toString)
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import qualified Data.UUID.V4  as UUID (nextRandom)

go :: IO ()
go = do
    eventId <- UUID.nextRandom
    let e = Envelope "2FooHappened" eventId "{\"foo\":\"bar\"}"
    conn <- connect
    append conn e

type EventType = String
type EventId = UUID.UUID

data Envelope =
    Envelope
        { eventType    :: EventType
        , eventId      :: EventId
        , eventPayload :: String
        }

connect :: IO Connection
connect = connectPostgreSQL "host=localhost dbname=allstreamtest user="

append :: Connection -> Envelope -> IO ()
append conn e = do
    stmt <- prepare conn "CALL append(?, ?, ?);" --event_type, event_id, event_payload
    execute
        stmt
        [toSql $ eventType e, toSql $ UUID.toString $ eventId e, toSql $ eventPayload e ]
    commit conn
    disconnect conn


module Domain.Process where

import Domain.Events 
import Domain.Commands
import Projection
import EventStore (IsDomainEvent)
import Database.HDBC.PostgreSQL (Connection)


handle :: Command -> Connection -> IO ()
handle (StartRound quizId ) conn = do

    --RoundHasStarted quizId
    --commit conn
    return ()



countEventsProjections = Projection {initState = 0, step = step', query = id}
step' n event = n + 1

module Domain.Process where

import           Control.Monad            (when)
import           Data.List                (foldl')
import           Database.HDBC.PostgreSQL (Connection)
import           Domain.Commands
import           Domain.Events
import           EventStore
import           Projection

handle :: Command -> Connection -> IO ()
handle (StartRound quizId) conn = do
    stream <- fetchStream conn "GameRound" quizId :: IO (Stream DomainEvent)
    let expectedStreamId = 1 + foldl' max 0 (streamSequenceNr <$> stream)
    appendToStream conn "GameRound" quizId expectedStreamId (RoundHasStarted quizId)
handle (JoinRound quizId playerId) conn = do
    stream <- fetchStream conn "GameRound" quizId :: IO (Stream DomainEvent)
    playerCount <- replay stream countPlayersInRound
    print playerCount
    
    let expectedStreamId = 1 + maximum (streamSequenceNr <$> stream)
    when (playerCount < 3) $
        appendToStream conn "GameRound" quizId expectedStreamId (PlayerHasJoined quizId playerId)
    when (playerCount >= 3) $
        appendToStream conn "GameRound" quizId expectedStreamId (RoundIsFull quizId playerId)
        
countPlayersInRound = Projection {initState = 0, step = step', query = id}

step' n PlayerHasJoined {} = n + 1
step' n _                  = n

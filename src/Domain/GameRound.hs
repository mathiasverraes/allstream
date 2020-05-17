module Domain.GameRound where

import           Control.Monad            (when)
import           Data.List                (foldl')
import           Domain.Commands
import           Domain.Events
import           EventStore
import           Projection

handle :: Command -> EventStore e -> IO ()
handle (StartRound quizId) es = do
    stream <- fetchStream es "GameRound" quizId :: IO (Stream DomainEvent)
    let expectedStreamId = 1 + foldl' max 0 (streamSeq <$> stream)
    appendToStream es "GameRound" quizId expectedStreamId (RoundHasStarted quizId)
handle (JoinRound quizId playerId) es = do
    stream <- fetchStream es "GameRound" quizId :: IO (Stream DomainEvent)
    playerCount <- replay stream countPlayersInRound
    let expectedStreamId = 1 + maximum (streamSeq <$> stream)
    when (playerCount < 3) $
        appendToStream es "GameRound" quizId expectedStreamId (PlayerHasJoined quizId playerId)
    when (playerCount >= 3) $
        appendToStream es "GameRound" quizId expectedStreamId (RoundIsFull quizId playerId)

countPlayersInRound = Projection {initState = 0, step = when_, query = id}

when_ PlayerHasJoined {} n = n + 1
when_ _ n                  = n

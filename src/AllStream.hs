module AllStream
    ( go
    ) where

import qualified Data.UUID.V4    as UUID (nextRandom)
import           Domain.Commands (Command (..))
import           Domain.Events
import           Domain.GameRound
import           EventStore


go :: IO ()
go = do
    conn <- connect
    quizId1 <- UUID.nextRandom
    --quizId2 <- UUID.nextRandom
    playerId1 <- UUID.nextRandom
    playerId2 <- UUID.nextRandom
    playerId3 <- UUID.nextRandom
    playerId4 <- UUID.nextRandom
    --
    handle (StartRound quizId1) conn
    handle (JoinRound quizId1 playerId1) conn
    handle (JoinRound quizId1 playerId2) conn
    handle (JoinRound quizId1 playerId3) conn
    handle (JoinRound quizId1 playerId4) conn
    --let outcome = handle command ()
    {-
    appendToStream conn "GameRound" quizId1 1 (RoundHasStarted quizId1)
    appendToStream conn "GameRound" quizId2 1 (RoundHasStarted quizId2)
    appendToStream conn "GameRound" quizId1 2 (PlayerHasJoined quizId1 playerId1)
    appendToStream conn "GameRound" quizId2 2 (PlayerHasJoined quizId2 playerId1)
    appendToStream conn "GameRound" quizId1 3 (PlayerHasJoined quizId1 playerId2)
    -}
    --
    stream' <- fetchAll conn :: IO (Stream DomainEvent)
    mapM_ print stream'
    --
    -- stream2' <- fetchStream conn "GameRound" quizId1 :: IO (Stream DomainEvent)
    -- mapM_ print (eventPayload <$> stream2')
    --
    --return ()

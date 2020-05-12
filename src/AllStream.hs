module AllStream
    ( go
    ) where

import qualified Data.UUID       as UUID
import qualified Data.UUID.V4    as UUID (nextRandom)
import           Domain.Commands (Command (..))
import           Domain.Events
import           Domain.Process
import           EventStore
import           Projection



go :: IO ()
go = do
    conn <- connect
    quizId1 <- UUID.nextRandom
    quizId2 <- UUID.nextRandom
    playerId1 <- UUID.nextRandom
    playerId2 <- UUID.nextRandom

    --let command = CreateQuiz quizId
    --let outcome = handle command ()

    let events = [ RoundHasStarted quizId1 ,PlayerHasJoined quizId1 playerId1 ,PlayerHasJoined quizId1 playerId2 ,RoundHasStarted quizId2 ,PlayerHasJoined quizId2 playerId1 ]

    registerStreamType conn "GameRound"
    startNewStream conn "GameRound" quizId1
    startNewStream conn "GameRound" quizId2
    --
    registerStreamType conn "Player"
    startNewStream conn "Player" playerId1
    startNewStream conn "Player" playerId2
    --
    appendToStream conn "GameRound" quizId1 1 (RoundHasStarted quizId1)
    appendToStream conn "GameRound" quizId1 2 (PlayerHasJoined quizId1 playerId1)
    appendToStream conn "GameRound" quizId1 3 (PlayerHasJoined quizId1 playerId2)
    appendToStream conn "GameRound" quizId2 1 (RoundHasStarted quizId2)
    appendToStream conn "GameRound" quizId2 2 (PlayerHasJoined quizId2 playerId1)

    --
    stream' <- stream conn :: IO (Stream DomainEvent)
    response <- replay stream' countEventsProjections
    putStrLn "Number of events: "
    print response
    return ()

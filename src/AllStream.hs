module AllStream
    ( go
    ) where

import           CommandHandler
import qualified Data.UUID.V4     as UUID (nextRandom)
import           Domain.Commands  (Command (..))
import           Domain.Events
import           Domain.GameRound
import           EventStore

go :: IO ()
go = do
    eventStore <- connect
    quizId1 <- UUID.nextRandom
    --quizId2 <- UUID.nextRandom
    playerId1 <- UUID.nextRandom
    playerId2 <- UUID.nextRandom
    playerId3 <- UUID.nextRandom
    playerId4 <- UUID.nextRandom
    --
    let handle = makeCmdHdlr gameRound eventStore
    handle (StartRound quizId1)
    handle (JoinRound quizId1 playerId1)
    handle (JoinRound quizId1 playerId2)
    handle (JoinRound quizId1 playerId3)
    handle (JoinRound quizId1 playerId4)

    stream' <- fetchAll eventStore :: IO (Stream DomainEvent)
    mapM_ print stream'


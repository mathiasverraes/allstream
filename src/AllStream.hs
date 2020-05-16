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
import Data.Maybe (fromJust)



go :: IO ()
go = do
    conn <- connect
    
    --quizId1 <- UUID.nextRandom
    let quizId1 = fromJust $ UUID.fromString "60991a24-e59a-4fa5-9b73-ac2a6b1fda27"
    quizId2 <- UUID.nextRandom
    playerId1 <- UUID.nextRandom
    playerId2 <- UUID.nextRandom

    --let command = StartRound quizId1
    --let outcome = handle command ()
    
    --- appendToStream :: (IsDomainEvent event) => Connection -> StreamType -> StreamId -> Int -> event -> IO ()
    appendToStream conn "GameRound" quizId1 1 (RoundHasStarted quizId1)
    appendToStream conn "GameRound" quizId2 1 (RoundHasStarted quizId2)
    appendToStream conn "GameRound" quizId1 2 (PlayerHasJoined quizId1 playerId1)
    appendToStream conn "GameRound" quizId2 2 (PlayerHasJoined quizId2 playerId1)
    appendToStream conn "GameRound" quizId1 3 (PlayerHasJoined quizId1 playerId2)

    --
    stream' <- fetchAll conn :: IO (Stream DomainEvent)
    mapM_ print stream'
    response <- replay stream' countEventsProjections
    putStrLn "Number of events: "
    print response
    --
    
    stream2' <- fetchStream conn "GameRound" quizId1 :: IO (Stream DomainEvent)
    mapM_ print (eventPayload <$> stream2')
    --
    --return ()

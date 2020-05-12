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

setup :: IO ()
setup :: do
    conn <- connect
    
go :: IO ()
go = do
    conn <- connect
    quizId <- UUID.nextRandom
    let command = CreateQuiz quizId
    let outcome = handle command ()
    registerNewStream conn "GameRound" quizId
    appendToStream conn "GameRound" quizId 1 outcome
    stream' <- stream conn :: IO (Stream DomainEvent)
    response <- replay stream' countEventsProjections
    putStrLn "Number of events: "
    print response
    return ()

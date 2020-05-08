module AllStream
    ( go
    ) where

import           Domain.Commands          (Command(..))
import           Domain.Events
import           EventStore
import           Projection

go :: IO ()
go = do
    let command = CreateQuiz "myQuizId" "Category Theory" "Lambdaman"
    let outcome = handle command ()
    conn <- connect
    append conn outcome
    stream <- stream conn
    response <- replay stream countEvents
    putStrLn "Number of events: "
    print response
    return ()

handle :: Command -> state -> DomainEvent
handle (CreateQuiz quizId quizTitle ownerId) _state =
    QuizWasCreated quizId quizTitle ownerId



countEvents = Projection {initState = 0, step = step', query = id}
step' n event = n + 1

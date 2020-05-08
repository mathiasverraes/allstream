module AllStream
    ( go
    ) where

import qualified Data.HashMap.Strict      as HashMap

import           Data.Aeson               (toJSON)
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Domain.Commands          (Command(..))
import           Domain.Events
import           EventStore

go :: IO ()
go = do
    let command = CreateQuiz "xyz" "Statues of the World" "Jimmy"
    let outcome = handle command ()
    conn <- connect
    append conn outcome
    print <$> stream conn
    return ()

handle :: Command -> state -> DomainEvent
handle (CreateQuiz quizId quizTitle ownerId) _state =
    QuizWasCreated quizId quizTitle ownerId


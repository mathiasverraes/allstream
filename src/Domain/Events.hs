{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Domain.Events where

import           Data.Aeson
import           Domain.Types
import           EventStore
import           GHC.Generics

data DomainEvent
    = RoundHasStarted
          { quizId :: QuizId
          }
    | PlayerHasJoined
          { quizId   :: QuizId
          , playerId :: PlayerId
          }
    deriving (Show, Eq, Generic)



instance ToJSON DomainEvent

instance FromJSON DomainEvent

instance IsDomainEvent DomainEvent where
    eventType RoundHasStarted {..}  = "RoundHasStarted"
    eventType PlayerHasJoined {..}  = "PlayerHasJoined"
    --belongsToStreams (RoundHasStarted quizId) = [StreamId "GameRound" quizId]
    --belongsToStreams (PlayerHasJoined quizId playerId) = [StreamId "GameRound" quizId, StreamId "Player" playerId]
{-
    eventType PlayerHasRegistered {..}  = "PlayerHasRegistered"
    eventType QuestionAddedToQuiz {..}  = "QuestionAddedToQuiz"
    eventType QuizWasCreated {..}       = "QuizWasCreated"
    eventType GameWasOpened {..}        = "GameWasOpened"
    eventType QuestionWasAsked {..}     = "QuestionWasAsked"
    eventType TimerHasExpired {..}      = "TimerHasExpired"
    eventType GameWasStarted {..}       = "GameWasStarted"
    eventType GameWasFinished {..}      = "GameWasFinished"
    eventType QuizWasPublished {..}     = "QuizWasPublished"
    eventType PlayerJoinedGame {..}     = "PlayerJoinedGame"
    eventType AnswerWasGiven {..}       = "AnswerWasGiven"
    eventType QuestionWasCompleted {..} = "QuestionWasCompleted"
    eventType GameWasCancelled {..}     = "GameWasCancelled"
-}


{-
    = PlayerHasRegistered
          { playerId  :: PlayerId
          , lastName  :: String
          , firstName :: String
          }
    | QuestionAddedToQuiz
          { quizId     :: QuizId
          , questionId :: QuestionId
          , question   :: String
          , answer     :: String
          }
    | QuizWasCreated
          { quizId    :: QuizId
          , quizTitle :: String
          , ownerId   :: OwnerId
          }
    | GameWasOpened
          { quizId   :: QuizId
          , gameId   :: GameId
          , playerId :: PlayerId
          }
    | QuestionWasAsked
          { questionId :: QuestionId
          , gameId     :: GameId
          }
    | TimerHasExpired
          { questionId :: QuestionId
          , playerId   :: PlayerId
          , gameId     :: GameId
          }
    | GameWasStarted
          { gameId :: GameId
          }
    | GameWasFinished
          { gameId :: GameId
          }
    | QuizWasPublished
          { quizId :: QuizId
          }
    | PlayerJoinedGame
          { playerId :: PlayerId
          , gameId   :: GameId
          }
    | AnswerWasGiven
          { questionId :: QuestionId
          , playerId   :: PlayerId
          , gameId     :: GameId
          , answer     :: String
          }
    | QuestionWasCompleted
          { questionId :: QuestionId
          , gameId     :: GameId
          }
    | GameWasCancelled
          { gameId :: GameId
          }
          -}
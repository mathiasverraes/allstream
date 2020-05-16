{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Commands where

import           Domain.Types
import           GHC.Generics

import           Data.Aeson

data Command
    = StartRound
          { quizId :: QuizId
          }
    | JoinRound
          { quizId   :: QuizId
          , playerId :: PlayerId
          }
    deriving (Show, Eq, Generic)

instance ToJSON Command

instance FromJSON Command

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Commands where

import           Domain.Types
import           GHC.Generics

import           Data.Aeson

data Command =
    CreateQuiz
        { quizId :: QuizId
        }
    deriving (Show, Eq, Generic)

instance ToJSON Command

instance FromJSON Command

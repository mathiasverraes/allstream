module Domain.Types where

import qualified Data.UUID                as UUID (UUID)

type GameId = String

type PlayerId = UUID.UUID

type OwnerId = String

type QuestionId = String

type QuizId = UUID.UUID

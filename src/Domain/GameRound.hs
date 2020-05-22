{-# LANGUAGE NamedFieldPuns #-}

module Domain.GameRound
    ( gameRound
    ) where

import           CommandHandler
import qualified Domain.Commands as Cmd
import           Domain.Events
import           Projection

type State = Int

countPlayersInRound = Projection {initState = 0, step = when, transform = id}

when :: DomainEvent -> State -> State
when PlayerHasJoined {} n = n + 1
when _ n                  = n

gameRound =
    CommandHandlerSpec
        { getStreamId = Cmd.quizId
        , getStreamType = const "GameRound"
        , getProjection = const countPlayersInRound
        , getConstraint = const max3Players
        }

max3Players :: Cmd.Command -> State -> DomainEvent

max3Players (Cmd.StartRound quizId) _ = RoundHasStarted quizId
max3Players (Cmd.JoinRound quizId playerId) state =
    if state < 3
        then PlayerHasJoined quizId playerId
        else RoundIsFull quizId playerId


{-# LANGUAGE NamedFieldPuns #-}

module Domain.GameRound (handle) where

import           Data.List       (foldl')
import qualified Domain.Commands as Cmd
import           Domain.Events
import           EventStore
import           Flow
import           Projection

type State = Int

countPlayersInRound = Projection {initState = 0, step = when_, transform = id}

when_ :: DomainEvent -> State -> State
when_ PlayerHasJoined {} n = n + 1
when_ _ n                  = n

handle :: Cmd.Command  -> EventStore event -> IO ()
handle command es = do
    let streamId = command |> Cmd.quizId
    stream <- fetchStream es "GameRound" streamId :: IO (Stream DomainEvent)
    let state = replay stream countPlayersInRound
    let expectedStreamSeq = 1 + foldl' max 0 (streamSeq <$> stream)
    appendToStream es "GameRound" streamId expectedStreamSeq $ aggregateAction command state

aggregateAction :: Cmd.Command -> State -> DomainEvent
aggregateAction (Cmd.StartRound quizId) _ = RoundHasStarted quizId
aggregateAction (Cmd.JoinRound quizId playerId) state =
    if state < 3
        then PlayerHasJoined quizId playerId
        else RoundIsFull quizId playerId

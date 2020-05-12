module Domain.Process where

import Domain.Events 
import Domain.Commands
import Projection
import EventStore (IsDomainEvent) 


handle :: Command -> state -> DomainEvent
handle (CreateQuiz quizId ) _state =
    RoundHasStarted quizId



countEventsProjections = Projection {initState = 0, step = step', query = id}
step' n event = n + 1

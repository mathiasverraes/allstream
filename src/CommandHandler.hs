module CommandHandler where

import EventStore 
import qualified Domain.Commands as Cmd
import Projection
import Flow ((|>), (<|))
import Data.Foldable (foldl') 

data CommandHandlerSpec event state response =
    CommandHandlerSpec
        { getStreamId   :: Cmd.Command -> StreamId
        , getStreamType :: Cmd.Command -> StreamType
        , getProjection :: Cmd.Command -> Projection event state response
        , getConstraint :: Cmd.Command -> Cmd.Command -> response -> event
        }

makeCmdHdlr ::
       (Payload event)
    => CommandHandlerSpec event state response
    -> EventStore event
    -> Cmd.Command
    -> IO ()
makeCmdHdlr (CommandHandlerSpec getStreamId getStreamType getProjection getConstraint) es cmd = do
    let streamId = getStreamId cmd
    let streamType = getStreamType cmd
    let projection = getProjection cmd
    let constraint = getConstraint cmd
    stream <- fetchStream es streamType streamId
    let response = replay stream projection
    let expectedStreamSeq = 1 + foldl' max 0 (streamSeq <$> stream)
    let outcome = constraint cmd response
    appendToStream es streamType streamId expectedStreamSeq outcome

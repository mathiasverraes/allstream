module Experiments.Conc where

import           Control.Concurrent


data Item a = Item a (Stream a)
type Stream a = MVar (Item a)
data Channel a = Channel (MVar (Stream a)) (MVar (Stream a))

waitASecond = threadDelay (10 ^ 6)

newChan' :: IO (Channel a)
newChan' = do
    hole <- newEmptyMVar
    readEnd <- newMVar hole
    writeEnd <- newMVar hole
    return (Channel readEnd writeEnd)
writeChan' :: Channel a -> a -> IO ()
writeChan' (Channel _ writeEnd) val = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeEnd
    putMVar oldHole (Item val newHole)
    putMVar writeEnd newHole

readChan' :: Channel a -> IO a
readChan' (Channel readEnd _) = do
    oldStreamStart <- takeMVar readEnd
    (Item a newStreamStart) <- takeMVar oldStreamStart
    putMVar readEnd newStreamStart
    return a

test :: IO()
test = do
    lock <- newMVar ()
    let atomicPutStrLn str = withMVar lock (\_ -> putStrLn str)
    --
    myChan <- newChan'
    forkIO $ do
        writeChan' myChan "Hello"
        waitASecond
        readChan' myChan >>= atomicPutStrLn
    forkIO $ do
        writeChan' myChan "World"
        waitASecond
        readChan' myChan >>= atomicPutStrLn
        readChan' myChan >>= atomicPutStrLn
    return ()



conc :: IO ()
conc = do
    lock <- newMVar ()
    let atomicPutStrLn str = withMVar lock (\_ -> putStrLn str)
    atomicPutStrLn "done starting"
    threadDelay 1000000
    threadDelay 5000000
    return ()


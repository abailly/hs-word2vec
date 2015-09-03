module Concurrent(runInThreadPool) where

import Control.Monad(replicateM)
import Control.Concurrent.MVar(newEmptyMVar,
                               takeMVar,
                               putMVar,
                               MVar)
import Control.Concurrent(forkIO,
                          killThread)


-- | Continuously executes an action, feeding it arguments from a channel.
doDownload :: (a -> IO b)  -- ^Action to run
           -> MVar a      -- ^A "channel" to retrieve arguments
           -> MVar b      -- ^A "channel" to feed results
           -> IO ()
doDownload action inChan outChan = do
        docId <- takeMVar inChan
        f <- action docId
        putMVar outChan f
        doDownload action inChan outChan

-- |Run a thread pool for executing concurrent computations.
runInThreadPool :: Int        -- ^number of threads to run concurrently
                -> [ a ]      -- ^list of inputs to proceed
                -> (a -> IO b) -- ^Action to run in threads
                -> IO [ b ]   -- ^list of action results
runInThreadPool numThreads ids compute= do
  inChan  <- newEmptyMVar
  outChan <- newEmptyMVar
  tids <- replicateM numThreads (forkIO $ doDownload compute inChan outChan)
  forkIO $ mapM_ (putMVar inChan) ids
  files <- mapM (const $ takeMVar outChan) ids
  mapM_ killThread tids
  return files
          



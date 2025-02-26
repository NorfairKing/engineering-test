{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler
  ( main,
    runScheduler,
  )
where

import Conduit
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Conduit.Combinators as C
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import System.Environment
import System.IO
import Text.Read
import UnliftIO
import UnliftIO.Concurrent

main :: IO ()
main = do
  args <- getArgs
  let maxJobs = listToMaybe args >>= readMaybe
  runScheduler maxJobs stdin stdout

-- | Run the scheduler on a handle for inputs and a handle for outputs
--
-- The plan is to parse the input as lines of text and send them down a request
-- channel.
-- Any responses from the scheduler are send down a response channel and
-- rendered as output lines.
-- The worker will handle requests one by one and will asynchronously send
-- responses down the response channel.
--
-- Input/output format is text, we may want to use lines of JSON instead, or
-- even a binary protocol, depending on the usecase
runScheduler :: Maybe Int -> Handle -> Handle -> IO ()
runScheduler mMaxJobs inH outH = do
  requestChan <- newChan
  responseChan <- newChan
  -- FIXME this may not be a good idea afterall.
  -- What if data comes in much faster than the scheduler can handle it? Then
  -- memore just blows up because of all the requests in the channel.
  -- We could parse more lazily but it may involve more specialised code.
  let parser = runConduit (sourceHandle inH .| parseRequestsConduit requestChan)
  let renderer = runConduit (renderResponsesConduit responseChan .| sinkHandle outH)
  let worker = schedulerWorker mMaxJobs requestChan responseChan
  -- The parser finishes when the handle is out of input,
  -- The renderer works untill every last response is sent,
  -- Which means we need to wait for all there to be done before exiting.
  concurrently_ parser (concurrently_ worker renderer)

parseRequestsConduit :: Chan Request -> ConduitT ByteString Void IO ()
parseRequestsConduit requestChan = C.linesUnboundedAscii .| go
  where
    go = do
      mNext <- await
      case mNext of
        Nothing -> writeChan requestChan RequestDone
        Just req -> do
          -- FIXME: Here we ignore invalid requests.
          -- We could instead send an error back.
          traverse_ (writeChan requestChan) (parseRequest req)
          go

parseRequest :: ByteString -> Maybe Request
parseRequest sb = case SB8.words sb of
  ["start", identifier, typeStr] -> do
    taskType <- parseTaskType typeStr
    pure $ RequestStart identifier taskType
  ["cancel", identifier] -> do
    pure $ RequestCancel identifier
  _ -> Nothing

renderResponsesConduit :: Chan Response -> ConduitT () ByteString IO ()
renderResponsesConduit responseChan = go .| C.unlinesAscii
  where
    go = do
      -- Note that this will keep blocking on 'readChan' if the scheduler never
      -- sends a ResponseDone.
      response <- readChan responseChan
      Conduit.yield $ renderResponse response
      if response == ResponseDone
        then pure ()
        else go

renderResponse :: Response -> ByteString
renderResponse = \case
  ResponseJobStarting jobId -> "job starting: " <> jobId
  ResponseJobRunning jobId -> "job running: " <> jobId
  ResponseJobDone jobId -> "job done: " <> jobId
  ResponseJobCancelled jobId -> "job cancelled: " <> jobId
  ResponseDone -> "done"

schedulerWorker :: Maybe Int -> Chan Request -> Chan Response -> IO ()
schedulerWorker mMaxJobs requestChan responseChan = do
  let respond = writeChan responseChan
  jobsMapVar <- newTVarIO M.empty
  let go = do
        request <- readChan requestChan
        case request of
          RequestDone -> do
            -- Done receiving inputs
            -- Wait for all jobs to finish
            jobs <- readTVarIO jobsMapVar
            traverse_ wait jobs
            -- Send the last response
            respond ResponseDone
          RequestStart jobId taskType -> do
            jobsRunning <- length <$> readTVarIO jobsMapVar
            let enoughCapacity = maybe True (jobsRunning <=) mMaxJobs
            -- FIXME here we ignore any jobs above the maximum.
            -- We could send an error instead.
            if enoughCapacity
              then do
                -- FIXME If there are colliding job identifiers then the second
                -- will override the first in the map.
                -- This will make it impossible to cancel the first.
                -- We could send back an error instead.
                let recordJob threadId = atomically $ modifyTVar' jobsMapVar (M.insert jobId threadId)
                let unrecordJob = atomically $ modifyTVar' jobsMapVar (M.delete jobId)
                let jobThread = do
                      runJob responseChan jobId taskType
                        `finally` unrecordJob
                withAsync jobThread $ \jobThreadId -> do
                  -- FIXME: There's a space leak issue because of a race condition
                  -- in which it could be that the job finishes and is removed
                  -- before it can be recorded in the jobs map.
                  recordJob jobThreadId
                  go
              else go
          RequestCancel jobId -> do
            mJobThreadId <- M.lookup jobId <$> readTVarIO jobsMapVar
            -- FIXME: This will ignore cancel requests for unknown jobs
            -- We could respond with an error instead
            for_ mJobThreadId $ \jobThreadId -> do
              cancel jobThreadId
              respond $ ResponseJobCancelled jobId
            -- We don't need to unrecord the job here because we use `finally`
            -- in the job thread to do that.
            go
  go

runJob :: Chan Response -> JobId -> TaskType -> IO ()
runJob responseChan jobId taskType = do
  let jobAction = case taskType of
        TaskTypeBubble -> runBubble
        TaskTypeSquirrel -> runSquirrel
        TaskTypeUnicorn -> runUnicorn

  -- Mention that the job is starting
  respond $ ResponseJobStarting jobId
  -- Run the job at the same time as sending periodic status updates
  let jobStatusThread = forever $ do
        threadDelay 100_000
        respond $ ResponseJobRunning jobId
  -- jobStatusThread uses 'forever', so this will only run as long as jobAction
  -- does.
  race_ jobAction jobStatusThread
  -- Mention that the job is done
  respond $ ResponseJobDone jobId
  where
    respond = writeChan responseChan

data Request
  = RequestStart !JobId !TaskType
  | RequestCancel !JobId
  | RequestDone
  deriving (Show, Eq)

type JobId = ByteString

data TaskType
  = TaskTypeBubble
  | TaskTypeSquirrel
  | TaskTypeUnicorn
  deriving (Show, Eq)

parseTaskType :: ByteString -> Maybe TaskType
parseTaskType = \case
  "bubble" -> Just TaskTypeBubble
  "squirrel" -> Just TaskTypeSquirrel
  "unicorn" -> Just TaskTypeUnicorn
  _ -> Nothing

runBubble :: IO ()
runBubble = threadDelay 1_000_000

runSquirrel :: IO ()
runSquirrel = threadDelay 2_000_000

runUnicorn :: IO ()
runUnicorn = threadDelay 3_000_000

data Response
  = ResponseJobStarting !JobId
  | ResponseJobRunning !JobId
  | ResponseJobCancelled !JobId
  | ResponseJobDone !JobId
  | ResponseDone
  deriving (Show, Eq)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler
  ( main,
    runScheduler,
  )
where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Conduit.Combinators as C
import System.IO
import UnliftIO

-- |
--
-- Things to reconsider:
-- * Input/output format is text, we may want to use lines of JSON instead, or
--   even a binary protocol, depending on the usecase
main :: IO ()
main = runScheduler stdin stdout

-- | Run the scheduler on a handle for inputs and a handle for outputs
--
-- The plan is to parse the input as lines of text and send them down a request
-- channel.
-- Any responses from the scheduler are send down a response channel and
-- rendered as output lines.
-- The worker will handle requests one by one and will asynchronously send
-- responses down the response channel.
runScheduler :: Handle -> Handle -> IO ()
runScheduler inH outH = do
  requestChan <- newChan
  responseChan <- newChan
  let parser = runConduit (sourceHandle inH .| parseRequestsConduit requestChan)
  let renderer = runConduit (renderResponsesConduit responseChan .| sinkHandle outH)
  let worker = schedulerWorker requestChan responseChan
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
          case parseRequest req of
            Just request -> writeChan requestChan request
            Nothing -> liftIO $ putStrLn $ "Unknown request: " <> show req
          go

parseRequest :: ByteString -> Maybe Request
parseRequest sb = case SB8.words sb of
  ["start", identifier] -> Just $ RequestStart identifier
  _ -> Nothing

renderResponsesConduit :: Chan Response -> ConduitT () ByteString IO ()
renderResponsesConduit responseChan = go .| C.unlinesAscii
  where
    go = do
      response <- readChan responseChan
      yield $ renderResponse response
      if response == ResponseDone
        then pure ()
        else go

renderResponse :: Response -> ByteString
renderResponse = \case
  ResponseStarting jobId -> "starting " <> jobId
  ResponseDone -> "done"

schedulerWorker :: Chan Request -> Chan Response -> IO ()
schedulerWorker requestChan responseChan = go
  where
    respond = writeChan responseChan
    go = do
      request <- readChan requestChan
      case request of
        RequestDone -> respond ResponseDone
        RequestStart jobId -> do
          respond $ ResponseStarting jobId
          go

type JobId = ByteString

data Request
  = RequestStart JobId
  | RequestDone
  deriving (Show, Eq)

data Response
  = ResponseStarting JobId
  | ResponseDone
  deriving (Show, Eq)

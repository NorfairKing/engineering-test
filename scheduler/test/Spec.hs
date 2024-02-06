{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as SB
import Scheduler
import System.Process
import Test.Syd
import UnliftIO

main :: IO ()
main = sydTest $ describe "runScheduler" $ do
  it "can run and wait for any input" $ do
    (inH, _) <- createPipe
    (_, outH) <- createPipe
    let run = runScheduler Nothing inH outH
    result <- race run (threadDelay 100_000)
    result `shouldBe` Right ()

  it "can schedule a single job and wait for it to finish" $ do
    (inH, toH) <- createPipe
    (fromH, outH) <- createPipe
    let run = runScheduler Nothing inH outH
    SB.hPut toH "start example bubble\n"
    hClose toH
    run
    contents <- SB.hGetContents fromH
    contents `shouldBe` "job starting: example\njob running: example\njob running: example\njob running: example\njob running: example\njob running: example\njob running: example\njob running: example\njob running: example\njob running: example\njob done: example\ndone\n"

-- Read all elements from a chan.
-- This uses a timeout to sense when the chance is empty, so it can only be
-- used after all producing is done.
readAll :: Chan a -> IO [a]
readAll chan = go
  where
    go = do
      mResult <- timeout 10_000 $ readChan chan
      case mResult of
        Nothing -> pure []
        Just a -> do
          as <- go
          pure $ a : as

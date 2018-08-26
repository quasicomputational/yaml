{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Aeson.Internal (JSONPath, JSONPathElement(..))
import           Data.Conduit ((.|), ConduitM, runConduit)
import qualified Data.Conduit.List as CL
import           Control.Monad.Reader
import           Control.Concurrent

main :: IO ()
main = mainConduit
-- main = mainFoo


mainConduit :: IO ()
mainConduit = runReaderT (runConduit (foo 0)) ()

mainFoo :: IO ()
mainFoo = runReaderT (foo 0) ()

foo :: (MonadReader () m, MonadIO m) => Int -> m ()
foo !n
  | n > 1000000 = ask
  | otherwise = do
      -- liftIO yield
      local id (foo $ succ n)

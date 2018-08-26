{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Aeson.Internal (JSONPath, JSONPathElement(..))
import           Data.Conduit ((.|), ConduitM, runConduit)
import qualified Data.Conduit.List as CL
import           Control.Monad.Reader
import           Control.Concurrent

main :: IO ()
main = mainFoo


mainConduit :: IO ()
mainConduit = runReaderT (runConduit (foo 0)) [] >>= print . length

mainFoo :: IO ()
mainFoo = runReaderT (foo 0) [] >>= print . length

foo :: (MonadReader JSONPath m, MonadIO m) => Int -> m JSONPath
foo !n
  | n > 1000000 = ask
  | otherwise = do
      -- liftIO yield
      local (let !x = Index n in let !xs = (\ ys -> x : ys) in xs) (foo $ succ n)
      -- local (Index n :) (foo $ succ n)

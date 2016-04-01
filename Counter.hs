{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Counter where

import Servant
import Servant.Docs
import Servant.Server (serve)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar(..), readTVarIO, modifyTVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

newtype CounterVal = CounterVal { getCounterVal :: Int }
                     deriving (Show, Num, FromJSON, ToJSON, Generic)

type GetCounter = Get '[JSON] CounterVal
type StepCounter = "step" :> Post '[JSON] ()
type Counter = GetCounter :<|> StepCounter

counterAPI :: Proxy Counter
counterAPI = Proxy
-- | >>> putStrLn counterDocs
counterDocs :: String
counterDocs = markdown $ docs counterAPI

instance ToSample CounterVal where
  toSamples _ = [("First access",0),("After 1 step",1),("After 14 steps", 14)]

handleGetCounter :: TVar CounterVal -> Server GetCounter
handleGetCounter ctr = liftIO $ readTVarIO ctr

handleStepCounyer :: TVar CounterVal -> Server StepCounter
handleStepCounyer ctr = liftIO $ atomically $ modifyTVar ctr (+1)

handleCounter :: TVar CounterVal -> Server Counter
handleCounter ctr = handleGetCounter ctr
               :<|> handleStepCounyer ctr

-- | $ cabal exec -- runhaskell Counter.hs
--   and enter next command other shell prompt.
--   $ curl -X GET http://localhost:8081
--   $ curl -X POST http://localhost:8081/step
main :: IO ()
main = do
  initCtr <- newTVarIO 0 :: IO (TVar CounterVal)
  run 8081 (serve counterAPI (handleCounter initCtr))

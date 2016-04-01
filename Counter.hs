{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Counter where

import Servant
import Data.Aeson
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

newtype CounterVal = CounterVal { getCounterVal :: Int }
                     deriving (Show, Num, FromJSON, ToJSON)

type GetCounter = Get '[JSON] CounterVal
type StepCounter = "step" :> Post '[] ()
type Counter = GetCounter :<|> StepCounter


module Main where

import Counter
import Servant
import Servant.Server (serve)
import Control.Concurrent.STM.TVar (TVar(..), readTVarIO, modifyTVar, newTVarIO, writeTVar)
import Network.Wai.Handler.Warp (run)


-- | $ cabal exec -- runhaskell Counter.hs
--   and enter next command other shell prompt.
--   $ curl -X GET http://localhost:8081
--   $ curl -X POST http://localhost:8081/step
--   $ curl -X PUT -H 'Content-Type: application/json' -d '123' http://localhost:8081
main :: IO ()
main = do
  initCtr <- newTVarIO 0 :: IO (TVar CounterVal)
  run 8081 (serve counterAPI (handleCounter initCtr))

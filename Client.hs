module Main where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client
import Servant
import Servant.Client

import Counter (counterAPI, CounterVal(..))

type Handler a = ExceptT ServantError IO a

getAPIs :: IO (Handler CounterVal, Handler (), CounterVal -> Handler ())
getAPIs = do
  m <- newManager defaultManagerSettings
  let get :<|> step :<|> set = client counterAPI (BaseUrl Http "localhost" 8081 "") m
  return (get, step, set)

main :: IO ()
main = do
  (get, step, set) <- getAPIs
  runExceptT $ do
    n <- get
    liftIO $ print $ getCounterVal n
    step
    step
    n' <- get
    liftIO $ print $ getCounterVal n'
    set $ CounterVal (getCounterVal n' + 10)
    n'' <- get
    liftIO $ print $ getCounterVal n''
    return n''
  return ()

module Main where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client
import Servant
import Servant.Common.Req
import Servant.Client

import Counter (counterAPI, CounterVal(..))

get :: Manager -> BaseUrl -> ClientM CounterVal
step :: Manager -> BaseUrl -> ClientM ()
set :: CounterVal -> Manager -> BaseUrl -> ClientM ()
get :<|> step :<|> set = client counterAPI

getAPIs :: IO (Handler CounterVal, Handler (), CounterVal -> Handler ())
getAPIs = do
  m <- newManager defaultManagerSettings
  let get :<|> step :<|> set = undefined -- client counterAPI (BaseUrl Http "localhost" 8081 "") m
  return (get, step, set)

main :: IO ()
main = do
  m <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" 8081 ""
  runExceptT $ do
    n <- get m url
    liftIO $ print $ getCounterVal n
    step m url
    step m url
    n' <- get m url
    liftIO $ print $ getCounterVal n'
    set (CounterVal (getCounterVal n' + 10)) m url
    n'' <- get m url
    liftIO $ print $ getCounterVal n''
    return n''
  return ()

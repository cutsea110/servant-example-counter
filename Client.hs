module Client where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client
import Servant
import Servant.Client

import Counter (counterAPI, CounterVal(..))

type Handler a = ExceptT ServantError IO a

getAPIs :: IO (Handler CounterVal, Handler ())
getAPIs = do
  m <- newManager defaultManagerSettings
  let get :<|> step = client counterAPI (BaseUrl Http "localhost" 8081 "") m
  return (get, step)

main :: IO (Either ServantError CounterVal)
main = do
  (get, step) <- getAPIs
  runExceptT $ do
    n <- get
    liftIO $ print $ getCounterVal n
    step
    step
    n' <- get
    liftIO $ print $ getCounterVal n'
    return n'

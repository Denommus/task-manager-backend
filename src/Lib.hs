module Lib
    ( someFunc
    ) where
import Database
import Servant
import TaskApi
import Data.Acid (AcidState)
import Network.Wai.Handler.Warp (run)

apiProxy :: Proxy TaskAPI
apiProxy = Proxy

app :: AcidState Database -> Application
app database = serve apiProxy $ server database

someFunc :: IO ()
someFunc = do
  database <- openDatabase
  run 8080 $ app database

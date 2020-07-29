module TaskApi where

import Data.Text
import Servant.API
import Servant
import ViewModels.Task
import Database
import Data.Acid
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)

type TaskAPI = "tasks" :> Get '[JSON] [Task]
  :<|> "tasks" :> ReqBody '[JSON] NewTask :> Post '[JSON] [Task]
  :<|> "tasks" :> ReqBody '[JSON] Task :> Put '[JSON] [Task]
  :<|> "tasks" :> Delete '[JSON] [Task]

listTasks :: AcidState Database -> Handler [Task]
listTasks database = do
  tasks <- liftIO $ query database ListTasks 
  return $ V.toList tasks

addTask :: AcidState Database -> NewTask -> Handler [Task]
addTask database newTask = do
  liftIO $ update database $ AddTask newTask
  listTasks database

updateTask :: AcidState Database -> Task -> Handler [Task]
updateTask database task = do
  liftIO $ update database $ UpdateTask task
  listTasks database

deleteTasks :: AcidState Database -> Handler [Task]
deleteTasks database = do
  liftIO $ update database DeleteTasks
  listTasks database

server :: AcidState Database -> Server TaskAPI
server database =
  listTasks database
    :<|> addTask database
    :<|> updateTask database
    :<|> deleteTasks database

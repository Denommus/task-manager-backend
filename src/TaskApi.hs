module TaskApi where

import Data.Text
import Servant.API
import Servant
import ViewModels.Task
import Database
import Data.Acid
import Control.Monad.IO.Class (liftIO)

type TaskAPI = "tasks" :> Get '[JSON] [QueryTask]
  :<|> "tasks" :> ReqBody '[JSON] NewTask :> Post '[JSON] [QueryTask]
  :<|> "tasks" :> ReqBody '[JSON] ToggleTask :> Put '[JSON] [QueryTask]
  :<|> "tasks" :> Delete '[JSON] [QueryTask]

listTasks :: AcidState Database -> Handler [QueryTask]
listTasks database = liftIO $ query database ListTasks

addTask :: AcidState Database -> NewTask -> Handler [QueryTask]
addTask database newTask = do
  liftIO $ update database $ AddTask newTask
  listTasks database

updateTask :: AcidState Database -> ToggleTask -> Handler [QueryTask]
updateTask database task = do
  liftIO $ update database $ UpdateTask task
  listTasks database

deleteTasks :: AcidState Database -> Handler [QueryTask]
deleteTasks database = do
  liftIO $ update database DeleteTasks
  listTasks database

server :: AcidState Database -> Server TaskAPI
server database =
  listTasks database
    :<|> addTask database
    :<|> updateTask database
    :<|> deleteTasks database

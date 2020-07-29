{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database (
  Database (..),
  ListTasks (..),
  AddTask (..),
  UpdateTask (..),
  DeleteTasks (..),
  openDatabase
  ) where

import ViewModels.Task
import Data.Acid
import Data.SafeCopy
import Control.Monad.Reader (asks, ask)
import Control.Monad.State (get, put)
import Data.Maybe (maybe)
import qualified Data.Vector as V

newtype Database = Database { tasks :: V.Vector Task }

$(deriveSafeCopy 0 'base ''Database)

listTasks :: Query Database (V.Vector Task)
listTasks = asks tasks

addTask :: NewTask -> Update Database ()
addTask newTask = do
  Database tasks <- get
  let tId = maybe 1 ((+1) . taskId) $ tasks V.!? (V.length tasks - 1)
  let task = Task tId (newTaskName newTask) False
  put . Database $ tasks <> return task

updateTask :: Task -> Update Database ()
updateTask task = do
  Database tasks <- get
  let (first, second) = V.break (\x -> taskId x == taskId task) tasks
  put . Database $ first <> return task <> V.drop 1 second

deleteTasks :: Update Database ()
deleteTasks = do
  Database tasks <- get
  put . Database $ V.filter (not . taskFinished) tasks

openDatabase ::IO (AcidState Database)
openDatabase = openLocalState $ Database V.empty

$(makeAcidic ''Database ['listTasks, 'addTask, 'updateTask, 'deleteTasks])

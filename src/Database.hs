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
import qualified Data.Map.Strict as M

newtype Database = Database { tasks :: M.Map Integer Task }

$(deriveSafeCopy 0 'base ''Database)

listTasks :: Query Database [QueryTask]
listTasks = do
  Database tasks <- ask
  return $ taskToQueryTask <$> M.toAscList tasks
  where taskToQueryTask (tId, Task tName tFinished) = QueryTask tId tName tFinished

addTask :: NewTask -> Update Database ()
addTask (NewTask tName) = do
  Database tasks <- get
  let tId = maybe 1 ((+1) . fst) $ M.lookupMax tasks
  put $ Database $ M.insert tId (Task tName False) tasks

updateTask :: ToggleTask -> Update Database ()
updateTask (ToggleTask tId) = do
  Database tasks <- get
  put . Database $ M.adjust toggle tId tasks
  where toggle (Task tName tFinished) = Task tName $ not tFinished

deleteTasks :: Update Database ()
deleteTasks = do
  Database tasks <- get
  put . Database $ M.filter (not . taskFinished) tasks

openDatabase ::IO (AcidState Database)
openDatabase = openLocalState $ Database M.empty

$(makeAcidic ''Database ['listTasks, 'addTask, 'updateTask, 'deleteTasks])

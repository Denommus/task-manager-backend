{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Database (
  Database (..),
  ListTasks (..),
  AddTask (..),
  UpdateTask (..),
  DeleteTasks (..),
  tasks,
  openDatabase
  ) where

import ViewModels.Task
import Data.Acid
import Data.SafeCopy
import Control.Monad.Reader (asks, ask)
import Control.Monad.State (get, put)
import Data.Maybe (maybe)
import qualified Data.Map.Strict as M
import Control.Lens

newtype Database = Database { _tasks :: M.Map Integer Task }
$(makeLenses ''Database)

$(deriveSafeCopy 0 'base ''Database)

listTasks :: Query Database [QueryTask]
listTasks = do
  tks <- view tasks
  return $ taskToQueryTask <$> M.toAscList tks
  where taskToQueryTask (tId, Task tName tFinished) = QueryTask tId tName tFinished

addTask :: NewTask -> Update Database ()
addTask (NewTask tName) = do
  tks <- use tasks
  let tId = maybe 1 ((+1) . fst) $ M.lookupMax tks
  tasks . ix tId .= Task tName False

updateTask :: ToggleTask -> Update Database ()
updateTask (ToggleTask tId) = tasks . ix tId %= toggle
  where toggle (Task tName tFinished) = Task tName $ not tFinished

deleteTasks :: Update Database ()
deleteTasks = tasks %= M.filter (not . _taskFinished)

openDatabase ::IO (AcidState Database)
openDatabase = openLocalState $ Database M.empty

$(makeAcidic ''Database ['listTasks, 'addTask, 'updateTask, 'deleteTasks])

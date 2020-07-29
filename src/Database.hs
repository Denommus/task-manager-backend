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

newtype Database0 = Database0 { _tasks0 :: M.Map Integer Task }

$(deriveSafeCopy 0 'base ''Database0)


data Database = Database { _tasks :: M.Map Integer Task, _lastId :: Integer }
$(makeLenses ''Database)

$(deriveSafeCopy 1 'extension ''Database)

instance Migrate Database where
  type MigrateFrom Database = Database0
  migrate (Database0 tasks0) = Database {
    _tasks = tasks0,
    _lastId = maybe 1 ((+1) . fst) $ M.lookupMax tasks0
    }

listTasks :: Query Database [QueryTask]
listTasks = do
  tks <- view tasks
  return $ taskToQueryTask <$> M.toAscList tks
  where taskToQueryTask (tId, Task tName tFinished) = QueryTask tId tName tFinished

addTask :: NewTask -> Update Database ()
addTask (NewTask tName) = do
  tks <- use tasks
  tId <- (+1) <$> use lastId
  tasks %= M.insert tId (Task tName False)
  lastId .= tId

updateTask :: ToggleTask -> Update Database ()
updateTask (ToggleTask tId) = tasks . ix tId %= toggle
  where toggle (Task tName tFinished) = Task tName $ not tFinished

deleteTasks :: Update Database ()
deleteTasks = tasks %= M.filter (not . _taskFinished)

openDatabase ::IO (AcidState Database)
openDatabase = openLocalState $ Database M.empty 0

$(makeAcidic ''Database ['listTasks, 'addTask, 'updateTask, 'deleteTasks])

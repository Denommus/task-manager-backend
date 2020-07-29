{-# LANGUAGE TemplateHaskell #-}
module ViewModels.Task (
  Task (..),
  NewTask (..),
  ToggleTask (..),
  QueryTask (..),
  taskName,
  taskFinished,
  newTaskName,
  toggleTaskId,
  queryTaskId,
  queryTaskName,
  queryTaskFinished
  ) where

import Data.Text
import Data.Aeson (
  ToJSON,
  FromJSON,
  object,
  toJSON,
  parseJSON,
  toEncoding,
  withObject,
  (.=),
  (.:),
  pairs
  )
import Data.SafeCopy
import Control.Lens (makeLenses)


-- Exported View Models
data Task = Task {
  _taskName :: Text,
  _taskFinished :: Bool
  }
  deriving (Show)

newtype NewTask = NewTask {
  _newTaskName :: Text
  }
  deriving (Show)

newtype ToggleTask = ToggleTask {
  _toggleTaskId :: Integer
  }

data QueryTask = QueryTask {
  _queryTaskId :: Integer,
  _queryTaskName :: Text,
  _queryTaskFinished :: Bool
  }

-- Their JSON encoders and decoders

instance ToJSON Task where
  toJSON (Task tName tFinished) =
    object ["name" .= tName, "finished" .= tFinished]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task
    <$> v .: "name"
    <*> v .: "finished"

instance ToJSON NewTask where
  toJSON (NewTask tName) =
    object ["name" .= tName]

instance FromJSON NewTask where
  parseJSON = withObject "NewTask" $ \v -> NewTask
    <$> v .: "name"

instance ToJSON ToggleTask where
  toJSON (ToggleTask tId) =
    object ["id" .= tId]

instance FromJSON ToggleTask where
  parseJSON = withObject "toggleTask" $ \v -> ToggleTask
    <$> v .: "id"

instance ToJSON QueryTask where
  toJSON (QueryTask tId tName tFinished) =
    object ["id" .= tId, "name" .= tName, "finished" .= tFinished]

instance FromJSON QueryTask where
  parseJSON = withObject "Task" $ \v -> QueryTask
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "finished"

$(makeLenses ''Task)
$(makeLenses ''NewTask)
$(makeLenses ''ToggleTask)
$(makeLenses ''QueryTask)

$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''NewTask)
$(deriveSafeCopy 0 'base ''ToggleTask)
$(deriveSafeCopy 0 'base ''QueryTask)

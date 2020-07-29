{-# LANGUAGE TemplateHaskell #-}
module ViewModels.Task (
  Task (..),
  NewTask (..)
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


-- Exported View Models
data Task = Task {
  taskId :: Integer,
  taskName :: Text,
  taskFinished :: Bool
  }
  deriving (Show)

newtype NewTask = NewTask {
  newTaskName :: Text
  }
  deriving (Show)

-- Their JSON encoders and decoders

instance ToJSON Task where
  toJSON (Task tId tName tFinished) =
    object ["id" .= tId, "name" .= tName, "finished" .= tFinished]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "finished"

instance ToJSON NewTask where
  toJSON (NewTask tName) =
    object ["name" .= tName]

instance FromJSON NewTask where
  parseJSON = withObject "NewTask" $ \v -> NewTask
    <$> v .: "name"


$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''NewTask)

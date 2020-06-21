module Lib
    ( mapAt
    , toggleCompleted
    , Todo (..)
    , Event (..)
    , State (..)
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

data Todo = Todo
    { name :: Text
    , completed :: Bool
    }

data Event
    = TodoTextChanged Text
    | TodoSubmitted
    | TodoToggled Int
    | Closed

data State = State
    { todos :: Vector Todo
    , currentText :: Text
    }

mapAt :: Int -> (a -> a) -> Vector a -> Vector a
mapAt i f = Vector.modify (\v -> MVector.write v i . f =<< MVector.read v i)

toggleCompleted :: Todo -> Todo
toggleCompleted todo = todo { completed = not (completed todo)}
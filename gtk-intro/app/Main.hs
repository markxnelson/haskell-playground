{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Control.Monad (void)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data Event
    = TodoTextChanged Text
    | TodoSubmitted
    | Closed

data Todo = Todo
    { name :: Text
    }

data State = State
    { todos :: Vector Todo
    , currentText :: Text
    }

main :: IO ()
main = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = State {todos = mempty, currentText = mempty}
    }

view' :: State -> AppView Gtk.Window Event
view' s = bin
    Gtk.Window
    [ #title := "TodoGtk+"
    , on #deleteEvent (const (True, Closed))
    ]
    (container Gtk.Box
        [#orientation := Gtk.OrientationVertical]
        [todoList, newTodoForm]
    )
    where
        todoList = container Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            (fmap todoItem (todos s))
        todoItem todo = widget Gtk.Label [#label := name todo]
        newTodoForm = widget
            Gtk.Entry
            [ #text := currentText s
            , #placeholderText := "What needs to be done?"
            , onM #changed (fmap TodoTextChanged . Gtk.entryGetText)
            , on #activate TodoSubmitted
            ]

update' :: State -> Event -> Transition State Event
update' s e = case e of
    TodoTextChanged t -> Transition s {currentText = t} (pure Nothing)
    Closed -> Exit

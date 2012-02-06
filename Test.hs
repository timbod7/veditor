{-# LANGUAGE TypeFamilies #-}

module Test where

import Graphics.UI.Gtk

import UI
import GTK
import ErrVal

data StructTest = StructTest {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

structTest :: (UITK tk) => UI tk StructTest
structTest = struct (StructTest "" 0 0) [
      field "v1" (t_v1,\v a->a{t_v1=v}) stringEntry,
      field "v2" (t_v2,\v a->a{t_v2=v}) readEntry,
      field "v3" (t_v3,\v a->a{t_v3=v}) readEntry
    ]

data UnionTest = UT_V1 String
               | UT_V2 Int
               | UT_V3 StructTest
    deriving (Show)

unionTest :: (UITK tk) => UI tk UnionTest
unionTest = union (UT_V1 "") [
      ucase "v1" (\v -> case v of { (UT_V1 v) -> Just v; _ -> Nothing },UT_V1) stringEntry,
      ucase "v2" (\v -> case v of { (UT_V2 v) -> Just v; _ -> Nothing },UT_V2) readEntry,
      ucase "v3" (\v -> case v of { (UT_V3 v) -> Just v; _ -> Nothing },UT_V3) structTest
    ]

testui :: (Show a) => UI GTK a -> IO ()
testui uidef =  do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10, windowTitle := "Hello World" ]
  
  ui <- uiNew uidef

  table <- tableNew 3 2 False
  tableAttachDefaults table (ui_widget ui) 0 2 0 1

  button <- buttonNew
  set button [ buttonLabel := "Print" ]
  onClicked button $ do
      ea <- ui_get ui
      errval (\v -> print v)
             (\err -> putStrLn ("ERROR: " ++ err))
             ea
  tableAttachDefaults table button 1 2 1 2

  button <- buttonNew
  set button [ buttonLabel := "Reset" ]
  onClicked button $ ui_reset ui
  tableAttachDefaults table button 1 2 2 3


  set window [ containerChild := table ]

  widgetShowAll window
  mainGUI

test1 = testui structTest
test2 = testui unionTest
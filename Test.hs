{-# LANGUAGE TypeFamilies #-}

module Test where

import Graphics.UI.Gtk

import UI
import GTK
import ErrVal

data Test = Test {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

uiTest :: (UITK tk) => UI tk Test
uiTest = struct (Test "" 0 0) [
      field "v1" (t_v1,\v a->a{t_v1=v}) stringField,
      field "v2" (t_v2,\v a->a{t_v2=v}) intField,
      field "v3" (t_v3,\v a->a{t_v3=v}) intField
    ]

main :: IO ()
main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10, windowTitle := "Hello World" ]
  
  ui <- uiNew uiTest

  button <- buttonNew
  set button [ buttonLabel := "Print" ]
  onClicked button $ do
      ea <- ui_get ui
      errval (\v -> print v)
             (\err -> putStrLn ("ERROR: " ++ err))
             ea

  table <- tableNew 2 2 False
  tableAttachDefaults table (ui_widget ui) 0 2 0 1
  tableAttachDefaults table button 1 2 1 2

  set window [ containerChild := table ]

  widgetShowAll window
  mainGUI

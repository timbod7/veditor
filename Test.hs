module Test where

import Graphics.UI.Gtk

import UI
import GTK
import UIExamples
import ErrVal

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
test2 = testui structTest2
test3 = testui unionTest
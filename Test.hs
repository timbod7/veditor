module Test where

import Graphics.UI.Gtk

import Control.Monad

import UI
import GTK
import UIExamples
import ErrVal

testui title uidef = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    set window [ containerBorderWidth := 10, windowTitle := "Test UI" ]
    button <- buttonNew
    set button [ buttonLabel := "Go" ]
        
    ui <- uiNew uidef 
    (dialog,runDialog) <- modalDialogNew title ui [dialogOK,dialogReset,dialogCancel]

    on button buttonActivated $ do
       mv <- runDialog
       case mv of
         Nothing -> return ()
         (Just v) -> print v
       
    set window [ containerChild := button ]
    widgetShowAll window
    mainGUI

  where


test1 = testui "Test: structTest" structTest
test2 = testui "Test: structTest2" structTest2
test3 = testui "Test: unionTest" unionTest
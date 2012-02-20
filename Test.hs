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
        
    dialog <- modalDialogNew title uidef [dialogOK,dialogReset,dialogCancel]

    on button buttonActivated $ do
       mr <- md_run dialog
       maybeM mr $ \v -> print v
       
    set window [ containerChild := button ]
    widgetShowAll window
    mainGUI

testAll = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    set window [ containerBorderWidth := 10, windowTitle := "Test UI" ]
    vbox <- vBoxNew False 5
    let addTest title uidef = do
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew title uidef [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           mr <- md_run dialog
           maybeM mr $ \v -> print v
        containerAdd vbox button

    addTest "StructTest" structTest
    addTest "StructTest2" structTest2
    addTest "UnionTest" unionTest
    addTest "[StructTest]" listTest
    addTest "StructTest3" structTest3
       
    set window [ containerChild := vbox ]
    widgetShowAll window
    mainGUI

test2 = testui "Test: StructTest2" structTest2
test3 = testui "Test: UnionTest" unionTest
test4 = testui "Test: [StructTest]" listTest
test5 = testui "Test: StructTest3" structTest3

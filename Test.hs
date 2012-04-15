module Test where

import Graphics.UI.Gtk
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as L

import Control.Monad

import UI
import GTK
import JSON
import UIExamples
import ErrVal

testui title uidef = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    set window [ containerBorderWidth := 10, windowTitle := "Test UI" ]
    button <- buttonNew
    set button [ buttonLabel := "Go" ]
        
    dialog <- modalDialogNew () title uidef [dialogOK,dialogReset,dialogCancel]

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
        let uig = uiGTK (ioFromConstUI uidef)
        let uij = uiJSON uidef
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew () title uig [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           mr <- md_run dialog
           maybeM mr $ \v -> do
               L.putStrLn (DA.encode (uj_tojson uij v))
        containerAdd vbox button

    addTest "StructTest" (mkUI :: UI ConstE StructTest)
    addTest "StructTest2" (mkUI :: UI ConstE StructTest2)
    addTest "UnionTest" (mkUI :: UI ConstE UnionTest)
    addTest "[StructTest]" listTest
    addTest "StructTest3" (mkUI :: UI ConstE StructTest3)
    addTest "Expr" (mkUI :: UI ConstE Expr)
       
    set window [ containerChild := vbox ]
    widgetShowAll window
    mainGUI

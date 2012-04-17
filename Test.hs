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
    let
      addTest :: String -> e -> UI (IOE e) a -> IO ()
      addTest title e uidef = do
        let uig = uiGTK uidef
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew e title uig [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           uidef' <- snapshotUI e uidef
           mr <- md_run dialog
           maybeM mr $ \v -> do
               L.putStrLn (DA.encode (uj_tojson (uiJSON uidef') v))
        containerAdd vbox button

      addConstTest title uidef = addTest title () (ioFromConstUI uidef)

    addConstTest "StructTest" (mkUI :: UI ConstE StructTest)
    addConstTest "StructTest2" (mkUI :: UI ConstE StructTest2)
    addConstTest "UnionTest" (mkUI :: UI ConstE UnionTest)
    addConstTest "[StructTest]" listTest
    addConstTest "StructTest3" (mkUI :: UI ConstE StructTest3)
    addConstTest "Expr" (mkUI :: UI ConstE Expr)
    addTest "EnvStruct" "/tmp" envStructUI
       
    set window [ containerChild := vbox ]
    widgetShowAll window
    mainGUI

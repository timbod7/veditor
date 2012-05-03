module Test(
 testAll,
 test1
) where

import Graphics.UI.Gtk
import qualified Data.Aeson as DA
import qualified Data.Aeson.Encode as DA
import qualified Data.Attoparsec.Text as DA
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy as Text

import Data.List
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

      -- This supports dynamic non-recursive uis
      addEnvTest :: String -> e -> UI (IOE e) a -> IO ()
      addEnvTest title e uidef = do
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

      -- This supports constant potentially recursive uis
      addConstTest :: String -> UI ConstE a -> IO ()
      addConstTest title uidef = do
        let uig = uiGTK (ioFromConstUI uidef)
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew () title uig [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           mr <- md_run dialog
           maybeM mr $ \v -> do
               L.putStrLn (DA.encode (uj_tojson (uiJSON uidef) v))
        containerAdd vbox button

    addConstTest "StructTest" (mkUI :: UI ConstE StructTest)
    addConstTest "StructTest2" (mkUI :: UI ConstE StructTest2)
    addConstTest "UnionTest" (mkUI :: UI ConstE UnionTest)
    addConstTest "[StructTest]" listTest
    addConstTest "StructTest3" (mkUI :: UI ConstE StructTest3)
    addConstTest "Expr" (mkUI :: UI ConstE Expr)
    addEnvTest "EnvStruct" "/tmp" envStructUI
       
    set window [ containerChild := vbox ]
    widgetShowAll window
    mainGUI

test1 :: UI ConstE a -> IO ()
test1 ui = do
    initGUI
    window <- windowNew
    vbox <- vBoxNew False 5
    hbox <- hBoxNew False 5
    onDestroy window mainQuit
    let uigtk = uiGTK (ioFromConstUI ui)
    let uijson = uiJSON ui
    set window [ containerBorderWidth := 10, windowTitle := ui_label uigtk ]
    uig <- ui_create uigtk (GTKCTX (return ()) ())
    boxPackStart vbox (ui_widget uig) PackGrow 0
    boxPackStart vbox hbox PackNatural 0
    set window [ containerChild := vbox ]
    cb <- clipboardGet selectionClipboard

    mkButton hbox "Reset" $ do
        ui_reset uig

    mkButton hbox "Paste" $ do
        clipboardRequestText cb $ \ms ->
          case ms of
            Nothing -> return ()
            (Just s) -> do
                case jsonFromString s of
                    Left _ -> return ()
                    Right v -> case uj_fromjson uijson v of
                        (EValue a) -> ui_set uig a
                        _ -> return ()

    mkButton hbox "Copy" $ do
        ev <- ui_get uig
        case ev of
            (Error emsg context) -> putStrLn (errorMessage emsg context)
            (EValue v) -> do
                let s = Text.unpack (Text.toLazyText (DA.fromValue (uj_tojson uijson v)))
                clipboardSetText cb s

    mkButton hbox "Get" $ do
        ev <- ui_get uig
        case ev of
            (Error emsg context) -> putStrLn (errorMessage emsg context)
            (EValue v) -> do
                let lbs = DA.encode (uj_tojson uijson v)
                L.putStrLn lbs

    widgetShowAll window
    mainGUI
  where
    errorMessage emsg context = "Error: " ++ emsg ++ " for " ++ (intercalate "." (reverse context))

    mkButton hbox label action = do
        b <- buttonNew
        set b [buttonLabel:=label]
        boxPackEnd hbox b PackNatural 0
        on b buttonActivated action


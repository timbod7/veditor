module Examples.Utils(
 testC,
 testE
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

import VE
import GTK
import JSON
import ErrVal

testC :: VE ConstE a -> IO ()
testC ui = do
    initGUI
    let uigtk = uiGTK (ioFromConstVE ui)
    uig <- ui_create uigtk (GTKCTX (return ()) ())
    test0 (ui_label uigtk) uig (uiJSON ui)

testE :: VE (IOE e) a -> e -> IO ()
testE ui e = do
    initGUI
    let uigtk = uiGTK ui
    uijson <- fmap uiJSON (snapshotVE e ui)
    uig <- ui_create uigtk (GTKCTX (return ()) e)
    test0 (ui_label uigtk) uig uijson

test0 :: String -> GTKWidget e1 a1 -> VEJSON a1 -> IO ()
test0 label uig uijson = do
    window <- windowNew
    vbox <- vBoxNew False 5
    hbox <- hBoxNew False 5
    onDestroy window mainQuit
    set window [ containerBorderWidth := 10, windowTitle := label ]
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

errorMessage emsg context = "Error: " ++ emsg ++ " for " ++ (intercalate "." (reverse context))

mkButton hbox label action = do
    b <- buttonNew
    set b [buttonLabel:=label]
    boxPackEnd hbox b PackNatural 0
    on b buttonActivated action


module Examples.All where

import Graphics.UI.Gtk
import qualified Data.Aeson as DA
import qualified Data.Aeson.Encode as DA
import qualified Data.Attoparsec.Text as DA
import qualified Data.ByteString.Lazy as L

import VE
import GTK
import JSON
import ErrVal

import qualified Examples.Example1 as Ex1
import qualified Examples.Example2 as Ex2
import qualified Examples.Example3 as Ex3
import qualified Examples.Example4 as Ex4
import qualified Examples.Example5 as Ex5


test = do
    initGUI
    window <- windowNew
    onDestroy window mainQuit
    set window [ containerBorderWidth := 10, windowTitle := "Test UI" ]
    vbox <- vBoxNew False 5
    let

      -- This supports dynamic non-recursive uis
      addEnvTest :: String -> e -> VE (IOE e) a -> IO ()
      addEnvTest title e uidef = do
        let uig = uiGTK uidef
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew e title uig [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           uidef' <- snapshotVE e uidef
           mr <- md_run dialog
           maybeM mr $ \v -> do
               L.putStrLn (DA.encode (uj_tojson (uiJSON uidef') v))
        containerAdd vbox button

      -- This supports constant potentially recursive uis
      addConstTest :: String -> VE ConstE a -> IO ()
      addConstTest title uidef = do
        let uig = uiGTK (ioFromConstVE uidef)
        button <- buttonNew
        set button [ buttonLabel := title ]
        dialog <- modalDialogNew () title uig [dialogOK,dialogReset,dialogCancel]
        on button buttonActivated $ do
           mr <- md_run dialog
           maybeM mr $ \v -> do
               L.putStrLn (DA.encode (uj_tojson (uiJSON uidef) v))
        containerAdd vbox button

    addConstTest "Example 1" (mkVE :: VE ConstE Ex1.Person)
    addConstTest "Example 2" (mkVE :: VE ConstE Ex2.Person)
    addConstTest "Example 3" (mkVE :: VE ConstE Ex3.Team)
    addConstTest "Example 4" (mkVE :: VE ConstE Ex4.Expr)
    addEnvTest "Example 5" "/tmp" Ex5.envStructVE
       
    set window [ containerChild := vbox ]
    widgetShowAll window
    mainGUI

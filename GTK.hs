{-# LANGUAGE TypeFamilies #-}

module GTK(
    GTK,
    GTKWidget(..),
    uiNew,
) where

import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative

import UI
import ErrVal

data GTK = GTK

data CTXState = CS_NORMAL
              | CS_AND {cs_table :: Table, cs_row :: Int}

data GTKCTX = GTKCTX {cs_state :: CTXState}

data GTKWidget a = GTKWidget {
    ui_widget :: Widget,
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a),
    ui_reset :: IO ()
}

instance UITK GTK where

    data UI GTK a = UIGTK {
        ui_label :: String,
        ui_create :: (GTKCTX -> IO (GTKWidget a))
    }

    entry = gtkEntry
    label s ui = ui{ui_label=s}
    nilUI = gtkNilUI 
    andUI = gtkAndUI
    orUI = gtkOrUI
    listUI = gtkListUI
    mapUI = gtkMapUI


gtkMapUI :: (a -> ErrVal b) -> (b -> a) -> UI GTK a -> UI GTK b
gtkMapUI fab fba (UIGTK label uia) = UIGTK label mkui
      where
        mkui ctx = do
            uiwa <- uia ctx
            return uiwa {
              ui_set=(ui_set uiwa).fba,
              ui_get=do
              fmap (errval fab eErr) (ui_get uiwa)
            }

invalidEntryBackground = Color 65535 50000 50000

gtkEntry :: (String -> ErrVal a) -> (a -> String) -> UI GTK a
gtkEntry fromString toString = UIGTK "" $ \ctx -> do
        e <- entryNew
        e `on` editableChanged $ setBackground e
        setBackground e
        return GTKWidget {
           ui_widget = toWidget e,
           ui_set = entrySetText e.toString,
           ui_get = fmap fromString (entryGetText e),
           ui_reset = entrySetText e ""
        } 
      where
        setBackground e = do
            s <-entryGetText e
            errval (\_ -> setOK e) (\_ -> setInvalid e) (fromString s)
        setOK e = widgetRestoreBase e StateNormal
        setInvalid e = widgetModifyBase e StateNormal invalidEntryBackground

gtkAndUI :: (HProduct l) => UI GTK a -> UI GTK l -> UI GTK (HAnd a l)
gtkAndUI ui uis = UIGTK "" $ \ctx -> do
        (table,i) <- case cs_state ctx of
            (CS_AND table i) -> do
                tableResize table (i + 1) 2
                return (table,i+1)
            CS_NORMAL -> do
                table <- tableNew 1 2 False
                return (table,0)

        -- create this field
        label <- labelNew (Just (ui_label ui))
        gw <- ui_create ui ctx{cs_state=CS_NORMAL}
        tableAttachDefaults table label 0 1 i (i+1) 
        tableAttachDefaults table (ui_widget gw) 1 2 i (i+1)

        -- create the subsequent rows
        gw2 <- ui_create uis ctx{cs_state=CS_AND table (i+1)}

        return GTKWidget {
           ui_widget = toWidget table,
           ui_set = \(a :&: b) -> ui_set gw a >> ui_set gw2 b,
           ui_get = do
               a <- ui_get gw
               b <- ui_get gw2
               return ((:&:) <$> a <*> b),
           ui_reset = ui_reset gw >> ui_reset gw2
        } 

gtkNilUI :: UI GTK HNil
gtkNilUI = UIGTK "" $ \ctx -> do
        w <- case cs_state ctx of
            (CS_AND table i) -> do
                return (toWidget table)
            CS_NORMAL -> do
                label <- labelNew Nothing
                return (toWidget label)
        return GTKWidget {
            ui_widget = w,
            ui_set = const $ return (),
            ui_get = return (eVal HNil),
            ui_reset = return ()
        } 

gtkOrUI :: (HSum l) => UI GTK a -> UI GTK l -> UI GTK (HOr a l)
gtkOrUI = undefined

gtkListUI :: UI GTK a -> UI GTK [a]
gtkListUI = undefined

uiNew :: (UI GTK a) -> IO (GTKWidget a)
uiNew (UIGTK _ uia) = uia (GTKCTX CS_NORMAL)
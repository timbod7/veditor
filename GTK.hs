{-# LANGUAGE TypeFamilies #-}

module GTK where

import Graphics.UI.Gtk
import Control.Monad

import UI
import ErrVal

data GTKUI = GTKUI

data GTKField a =  GTKField {
    gtkf_label :: String,
    gtkf_widget :: Widget, 
    gtkf_setf  :: UICC GTKUI -> a -> IO (),
    gtkf_updatef :: UICC GTKUI -> IO (a->a)
    }

instance UITK GTKUI where
    data UIW GTKUI = GTKUIW Widget
    data UICC GTKUI = GTKUICC

    data Field GTKUI a = GTKF (UICC GTKUI -> IO (GTKField a))

    -- stringField :: UICC -> UIWidget GTKUI String
    stringField ctx = do
        e <- entryNew
        return UIWidget {
           ui_widget = GTKUIW (toWidget e),
           ui_set = entrySetText e,
           ui_get = fmap eVal (entryGetText e)
        } 

    -- field :: String -> (a->f,f->a->a) -> UI TTK f -> Field UITK a
    field label (rf,uf) uif = GTKF undefined
      where
        setf ctx a = do
            ui <- uif ctx
            ui_set ui (rf a)
        updatef =undefined

    -- struct :: a -> [Field UITk a] -> UI UITK a

    struct defv fields ctx = do
        table <- tableNew (length fields) 2 False
--        forM_ (zip fields [0,1..]) $ \(f,i) -> do
--              label <- labelNew (Just (gtkf_label f))
--              tableAttachDefaults table label 0 1 i (i+1) 
        return UIWidget {
           ui_widget = undefined,
           ui_set = undefined,
           ui_get = undefined
        }

    list = undefined
    union = undefined

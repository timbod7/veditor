{-# LANGUAGE TypeFamilies #-}

module GTK where

import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative

import UI
import ErrVal

data GTK = GTK
data GTKCTX = GTKCTX

data GTKWidget a = GTKWidget {
    ui_widget :: Widget,
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a),
    ui_reset :: IO (),
    ui_indicateValid :: Bool -> IO ()
}

data GTKField a =  GTKField {
    gtkf_label :: String,
    gtkf_widget :: Widget,
    gtkf_setf  :: a -> IO (),
    gtkf_getf :: IO (ErrVal (a->a)),
    gtkf_reset :: IO ()
    }

invalidEntryBackground = Color 65535 50000 50000
validEntryBackground = Color 65535 65535 65535

instance UITK GTK where
    data UI GTK a = UIGTK (GTKCTX -> IO (GTKWidget a))
    data Field GTK a = UIField (GTKCTX -> IO (GTKField a))

    stringField = UIGTK $ \ctx -> do
        e <- entryNew
        return GTKWidget {
           ui_widget = toWidget e,
           ui_set = entrySetText e,
           ui_get = fmap eVal (entryGetText e),
           ui_reset = entrySetText e "",
           ui_indicateValid = \v ->
               if v then widgetModifyBase e StateNormal invalidEntryBackground
                    else widgetModifyBase e StateNormal validEntryBackground
        } 

    struct defv fields = UIGTK $ \ctx -> do
        table <- tableNew (length fields) 2 False
        let setf0 a = return ()
        let getf0 = return (eVal defv)
        let resetf0 = return ()
        (_,setf,getf,resetf) <- foldM (addField ctx table)
                                      (0,setf0,getf0,resetf0) fields
        return GTKWidget {
            ui_widget = toWidget table,
            ui_set = setf,
            ui_get = getf,
            ui_reset = resetf,
            ui_indicateValid = (\v -> return ())
        }
      where
        addField ctx table (i,setf,getf,resetf) (UIField ff) = do
            f <- ff ctx
            label <- labelNew (Just (gtkf_label f))
            tableAttachDefaults table label 0 1 i (i+1) 
            tableAttachDefaults table (gtkf_widget f) 1 2 i (i+1)
            return (i+1,setf' f,getf' f, resetf' f)
          where
            setf' f a = gtkf_setf f a >> setf a
            getf' f = do
                ea <- getf
                egf <- gtkf_getf f
                return (egf <*> ea)
            resetf' f = resetf >> gtkf_reset f

    field label (rf,uf) (UIGTK uif) = UIField $ \ ctx -> do
        ui <- uif ctx
        return GTKField {
            gtkf_label=label,
            gtkf_widget=ui_widget ui,
            gtkf_setf=ui_set ui.rf,
            gtkf_getf=do
                ef <- ui_get ui
                return (uf <$> ef),
            gtkf_reset=ui_reset ui                       
        }

    list = undefined
    union = undefined

    mapUI fab fba (UIGTK uia) = UIGTK mkui
      where
        mkui ctx = do
            uiwa <- uia ctx
            return uiwa {
              ui_set=(ui_set uiwa).fba,
              ui_get=do
              fmap (errval fab eErr) (ui_get uiwa)
            }


uiNew :: (UI GTK a) -> IO (GTKWidget a)
uiNew (UIGTK uia) = uia GTKCTX
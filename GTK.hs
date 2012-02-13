{-# LANGUAGE TypeFamilies #-}

module GTK(
    GTK,
    GTKWidget(..),
    uiNew,
    modalDialogNew,
    dialogOK, dialogReset, dialogCancel
) where

import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.Map as Map

import UI
import ErrVal

data GTK = GTK

data UnionState = UnionState {
    us_table :: Table,
    us_combo :: ComboBox,
    us_current :: IORef (Maybe (Int,Widget)),
    us_callback :: IORef (Map.Map Int (IO ())),
    us_i :: Int
}

data CTXState = CS_NORMAL
              | CS_AND Table Int
              | CS_OR UnionState

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
                tableSetColSpacing table 0 10
                return (table,0)

        -- create this field
        label <- labelNew (Just (ui_label ui))
        miscSetAlignment label 1 0
        gw <- ui_create ui ctx{cs_state=CS_NORMAL}
        let toptions = [Expand,Shrink,Fill]
        tableAttach table label 0 1 i (i+1) [] [] 0 0
        tableAttach table (ui_widget gw) 1 2 i (i+1) toptions toptions 0 0

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
            (CS_OR us) -> do
                return (toWidget (us_table us))
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
gtkOrUI ui uis = UIGTK "" $ \ctx -> do
        us <- case cs_state ctx of
            (CS_OR us) -> return us
            CS_NORMAL -> do
                table <- tableNew 1 2 False
                combo <- comboBoxNewText
                cref <- newIORef Map.empty
                on combo changed $ do
                    i <- comboBoxGetActive combo
                    cm <- readIORef cref
                    case Map.lookup i cm of
                        Nothing -> return ()
                        (Just a) -> a
                    return ()

                align <- alignmentNew 0 0 0 0
                containerAdd align combo
                tableAttachDefaults table align 0 1 0 1

                wref <- newIORef Nothing
                return (UnionState table combo wref cref 0)

        let (UnionState table combo wref cref i) = us

        comboBoxAppendText combo (ui_label ui)

        dgw <- delayIO (ui_create ui) ctx{cs_state=CS_NORMAL}

        gw2 <- ui_create uis ctx{cs_state=CS_OR us{us_i=i+1}}

        let showThisUI = do
              gw <- delayGet dgw
              mw <- readIORef wref
              case mw of
                Nothing -> do
                    tableAttachDefaults table (ui_widget gw) 0 1 1 2
                    widgetShowAll (ui_widget gw)
                    writeIORef wref (Just (i,(ui_widget gw)))
                (Just (oi,w)) | oi == i -> return ()
                              | otherwise -> do
                    containerRemove table w
                    tableAttachDefaults table (ui_widget gw) 0 2 1 2
                    widgetShowAll (ui_widget gw)
                    writeIORef wref (Just (i,(ui_widget gw)))
              return gw

            set (HSkp a) = ui_set gw2 a
            set (HVal a) = do
              comboBoxSetActive combo i
              gw <- showThisUI
              ui_set gw a

            get = do
                i' <- comboBoxGetActive combo
                if i' == i
                  then do
                    gw <- delayGet dgw
                    a <- ui_get gw
                    return (fmap HVal a)
                  else do
                    b <- ui_get gw2
                    return (fmap HSkp b)

            reset = do
                when (i==0) $ do
                    comboBoxSetActive combo i
                pending <- delayPending dgw
                when (not pending) $ do
                    gw <- delayGet dgw
                    ui_reset gw
                ui_reset gw2

        modifyIORef cref (Map.insert i (void showThisUI))

        when (i==0) $ do
            comboBoxSetActive combo i
            void showThisUI

        return GTKWidget {
          ui_widget = toWidget table,
          ui_set = set,
          ui_get = get,
          ui_reset = reset
        } 

data Delayed a = Delayed {
      delayGet :: IO a,
      delayPending :: IO Bool
}

delayIO :: (c -> IO a) -> c -> IO (Delayed a)
delayIO f v = do
    r <- newIORef Nothing
    return (Delayed (get r) (pending r))
  where
    get r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return a
          Nothing -> do
            a <- f v
            writeIORef r (Just a)
            return a

    pending r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return False
          Nothing -> return True

gtkListUI :: UI GTK a -> UI GTK [a]
gtkListUI = undefined

uiNew :: (UI GTK a) -> IO (GTKWidget a)
uiNew (UIGTK _ uia) = uia (GTKCTX CS_NORMAL)

type DialogButton a b = (String,GTKWidget a -> IO (Maybe b))
modalDialogNew :: String -> GTKWidget a -> [DialogButton a b] -> IO (Dialog,IO b)
modalDialogNew title gw buttons = do
    dialog <- dialogNew
    resultv <- newIORef undefined
    set dialog [ windowTitle := title ]

    -- Populate the upper area
    vbox <- dialogGetUpper dialog
    align <- alignmentNew 0 0 1 1
    alignmentSetPadding align 10 10 10 10
    containerAdd align (ui_widget gw)
    
    boxPackStart vbox align PackGrow 0
    widgetShowAll vbox

    -- Create the buttons in the action area
    hbox <- dialogGetActionArea dialog
    forM_ buttons $ \(label, action) -> do
        b <- buttonNew
        set b [buttonLabel:=label]
        boxPackStart hbox b PackNatural 0
        on b buttonActivated $ do
            v <- action gw
            case v of
              Nothing -> return ()
              (Just b) -> do
                writeIORef resultv b
                dialogResponse dialog ResponseOk
        return ()
    widgetShowAll hbox

    return (dialog,run dialog resultv)
 where
    run dialog resultv = do
        widgetShow dialog
        r <- dialogRun dialog
        v <- readIORef resultv
        widgetHide dialog
        return v
        
dialogOK, dialogReset, dialogCancel :: DialogButton a (Maybe a)
dialogOK = ("OK",ok)
  where
    ok gw = do
        ev <- ui_get gw
        case ev of
           (ErrVal (Left v)) -> return (Just (Just v))
           (ErrVal (Right s)) -> return Nothing

dialogReset = ("Reset",reset)
  where
    reset gw = do
        ui_reset gw
        return Nothing

dialogCancel = ("Cancel",cancel)
  where
    cancel gw = do
        return (Just (Nothing))

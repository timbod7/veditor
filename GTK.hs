{-# LANGUAGE GADTs #-}

module GTK(
    GTKWidget(..),
    modalDialogNew,
    ModalDialog(..),
    dialogOK, dialogReset, dialogCancel,
    maybeM,
    uiGTK
) where

import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.Map as Map

import UI
import ErrVal

data GTKCTX = GTKCTX {
    cs_onActivate :: IO ()
}

data GTKWidget a = GTKWidget {
    ui_widget :: Widget,
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a),
    ui_reset :: IO (),
    ui_packWide :: Bool,
    ui_tableXAttach :: [AttachOptions],
    ui_tableYAttach :: [AttachOptions]
}

data UIGTK a = UIGTK {
    ui_label :: String,
    ui_create :: (GTKCTX -> IO (GTKWidget a))
}

uiGTK  :: UI a -> UIGTK a
uiGTK (Entry fromString toString) = gtkEntry fromString toString 
uiGTK (Label label ui) = (uiGTK ui){ui_label=label}
uiGTK (MapUI fab fba ui) = gtkMapUI fab fba (uiGTK ui)
uiGTK (DefaultUI a ui) = gtkDefaultUI a (uiGTK ui)
uiGTK (EnumUI ss) = gtkEnumUI ss
uiGTK (ListUI toString ui) = gtkListUI toString (uiGTK ui)
uiGTK (AndUI uia uib) = gtkAndUI uia uib
uiGTK (OrUI uia uib) = gtkOrUI uia uib

gtkAndUI :: (UI a) -> (UI b) -> UIGTK (a,b)
gtkAndUI uia uib = UIGTK "" $ \ctx -> do
    let ui = (AndUI uia uib)
    let nRows = countFields ui
    table <- tableNew nRows 2 False
    rowiv <- newIORef 0
    addFields table rowiv ctx ui 
  where
    countFields :: UI a -> Int
    countFields (AndUI uia uib) = countFields uia + countFields uib
    countFields _ = 1

    addFields :: Table -> IORef Int -> GTKCTX -> UI a -> IO (GTKWidget a)
    addFields table rowiv ctx (AndUI uia uib) = do
        gwa <- addFields table rowiv ctx uia
        gwb <- addFields table rowiv ctx uib
        return GTKWidget {
            ui_widget = toWidget table,
            ui_set = \(a,b) -> do
                ui_set gwa a
                ui_set gwb b,
            ui_get = do
                ea <- ui_get gwa
                eb <- ui_get gwb
                return ( (,) <$> ea <*> eb ),
            ui_reset = ui_reset gwa >> ui_reset gwb,
            ui_packWide = True,
            ui_tableXAttach = [],
            ui_tableYAttach = []
        } 

    addFields table rowiv ctx aui = do
        let ui = uiGTK aui
        gw <- ui_create ui ctx
        i <- readIORef rowiv
        modifyIORef rowiv (1+)
        let xattach = ui_tableXAttach gw
        let yattach = ui_tableYAttach gw
        if ui_packWide gw
          then do
            f <- frameNew
            frameSetLabel f (ui_label ui)
            containerAdd f (ui_widget gw)
            tableAttach table f 0 2 i (i+1) xattach yattach 0 0
          else do
            label <- labelNew (Just (ui_label ui))
            miscSetAlignment label 1 0
            tableAttach table label 0 1 i (i+1) [] [] 0 0
            tableAttach table (ui_widget gw) 1 2 i (i+1) xattach yattach 0 0
        return gw

gtkMapUI :: (a -> ErrVal b) -> (b -> a) -> UIGTK a -> UIGTK b
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

gtkEntry :: (String -> ErrVal a) -> (a -> String) -> UIGTK a
gtkEntry fromString toString = UIGTK "" $ \ctx -> do
    e <- entryNew
    e `on` editableChanged $ setBackground e
    e `on` entryActivate  $ cs_onActivate ctx
    setBackground e
    return GTKWidget {
        ui_widget = toWidget e,
        ui_set = entrySetText e.toString,
        ui_get = fmap fromString (entryGetText e),
        ui_reset = entrySetText e "",
        ui_packWide = False,
        ui_tableXAttach = [Expand,Shrink,Fill],
        ui_tableYAttach = []
        } 
  where
    setBackground e = do
        s <-entryGetText e
        errval (\_ -> setOK e) (\_ -> setInvalid e) (fromString s)
    setOK e = widgetRestoreBase e StateNormal
    setInvalid e = widgetModifyBase e StateNormal invalidEntryBackground

data UnionState = UnionState {
    us_table :: Table,
    us_combo :: ComboBox,
    us_current :: IORef (Maybe (Int,Widget)),
    us_callback :: IORef (Map.Map Int (IO ())),
    us_i :: IORef Int
}

gtkOrUI :: UI a -> UI b -> UIGTK (Either a b)
gtkOrUI uia uib = UIGTK "" $ \ctx -> do
    let ui = (OrUI uia uib)
    let labels = getLabels ui 
    frame <- frameNew
    table <- tableNew 1 1 False
    combo <- comboBoxNewText
    cref <- newIORef Map.empty
    on combo changed $ do
        i <- comboBoxGetActive combo
        cm <- readIORef cref
        case Map.lookup i cm of
            Nothing -> return ()
            (Just a) -> a
        return ()

    frameSetLabelWidget frame combo
    containerAdd frame table
    rowiv <- newIORef 0
    wref <- newIORef Nothing
    let ustate = UnionState table combo wref cref rowiv 

    gw <- addChoices ustate ctx ui
    return gw { 
        ui_widget = toWidget frame
    }

  where
    getLabels :: UI a -> [String]
    getLabels (OrUI uia uib) = getLabels uia ++ getLabels uib
    getLabels ui = [ui_label (uiGTK ui)]

    addChoices :: UnionState -> GTKCTX -> UI a -> IO (GTKWidget a)
    addChoices us ctx (OrUI uia uib) = do
        gwa <- addChoices us ctx uia
        ileft <- readIORef (us_i us)
        gwb <- addChoices us ctx uib

        let isLeftSelected = do
            mc <- readIORef (us_current us)
            case mc of
                Nothing -> error "impossible"
                (Just (i,_)) -> return (i < ileft)

        return GTKWidget {
            ui_widget = toWidget (us_table us),
            ui_set = \v -> case v of
                (Left a) -> ui_set gwa a
                (Right b) -> ui_set gwb b,
            ui_get = do
                l <- isLeftSelected
                if l then do ea <- ui_get gwa
                             return (fmap Left ea)
                     else do eb <- ui_get gwb
                             return (fmap Right eb)
                ,
            ui_reset = ui_reset gwa
                ,
            ui_packWide = False,
            ui_tableXAttach = [Expand,Shrink,Fill],
            ui_tableYAttach = []
        } 
        
    addChoices us ctx aui  = do
        let ui = uiGTK aui

        comboBoxAppendText (us_combo us) (ui_label ui)

        i <- readIORef (us_i us)
        modifyIORef (us_i us) (+1)

        dgw <- delayIO (ui_create ui ctx)

        let showThisUI = do
              let wref = us_current us
              let table = us_table us
              let attach w = tableAttach table w 0 1 0 1 [Expand,Fill] [Expand,Fill] 10 0
              gw <- delayGet dgw
              mw <- readIORef wref
              case mw of
                Nothing -> do
                  attach (ui_widget gw)
                  widgetShowAll (ui_widget gw)
                  writeIORef wref (Just (i,(ui_widget gw)))
                (Just (oi,w)) | oi == i -> return ()
                              | otherwise -> do
                  containerRemove table w
                  attach (ui_widget gw)
                  widgetShowAll (ui_widget gw)
                  writeIORef wref (Just (i,(ui_widget gw)))
              return gw

            setCombo = comboBoxSetActive (us_combo us) i

            set a = do
                setCombo
                gw <- showThisUI
                ui_set gw a

        modifyIORef (us_callback us) (Map.insert i (void showThisUI))

        when (i==0) $ do
            setCombo
            void showThisUI

        return GTKWidget {
            ui_widget = toWidget (us_table us),
            ui_set = set,
            ui_get = showThisUI >>= ui_get,
            ui_reset = setCombo >> showThisUI >>= ui_reset,
            ui_packWide = True,
            ui_tableXAttach = [Expand,Shrink,Fill],
            ui_tableYAttach = [Expand,Shrink,Fill]
         } 

data Delayed a = Delayed {
      delayGet :: IO a,
      delayPending :: IO Bool
}

delayIO :: IO a -> IO (Delayed a)
delayIO f = do
    r <- newIORef Nothing
    return (Delayed (get r) (pending r))
  where
    get r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return a
          Nothing -> do
            a <- f
            writeIORef r (Just a)
            return a

    pending r = do
        ma <- readIORef r
        case ma of
          (Just a) -> return False
          Nothing -> return True

gtkListUI :: (a -> String) -> UIGTK a -> UIGTK [a]
gtkListUI toString ui = UIGTK "" $ \ctx -> do
    ls <- listStoreNew []
    tree <- treeViewNewWithModel ls
    col1 <- treeViewColumnNew

    ddialog <- delayIO $ modalDialogNew "List Edit" ui
                                        [dialogOK,dialogReset,dialogCancel]

    renderer1 <- cellRendererTextNew
    cellLayoutPackStart col1 renderer1 True
    cellLayoutSetAttributes col1 renderer1 ls $ \a -> [cellText := toString a ]
    treeViewSetHeadersVisible tree False
    treeViewAppendColumn tree col1
    treeViewSetReorderable tree True
    on tree rowActivated $ \path column -> do
        miter <- treeModelGetIter ls path
        case miter of
            Nothing -> return ()
            Just (iter) -> do
                let i = listStoreIterToIndex iter
                v <- listStoreGetValue ls i
                dialog <- delayGet ddialog
                ui_set (md_gw dialog) v
                mr <- md_run dialog
                maybeM mr $ \v -> listStoreSetValue ls i v

    addButton <- stockButton stockAdd IconSizeButton $ do
        dialog <- delayGet ddialog
        mi <- getListSelection tree
        case mi of
            Nothing -> ui_reset (md_gw dialog)
            (Just i) -> do
               v <- listStoreGetValue ls i
               ui_set (md_gw dialog) v
        mr <- md_run dialog
        maybeM mr $ \v -> void $ listStoreAppend ls v

    deleteButton <- stockButton stockRemove IconSizeButton $ do
        mi <- getListSelection tree
        maybeM mi $ \i -> listStoreRemove ls i

    hbox <-hBoxNew False 5
    vbox <- vBoxNew False 5
    boxPackStart hbox tree PackGrow 0
    boxPackStart hbox vbox PackNatural 0
    boxPackStart vbox addButton PackNatural 0
    boxPackStart vbox deleteButton PackNatural 0
                                            
    return GTKWidget {
          ui_widget = toWidget hbox,
          ui_set = \vs -> do
              listStoreClear ls
              mapM_ (listStoreAppend ls) vs,
          ui_get = fmap eVal (listStoreToList ls),
          ui_reset = listStoreClear ls,
          ui_packWide = True,
          ui_tableXAttach = [Expand,Shrink,Fill],
          ui_tableYAttach = [Expand,Shrink,Fill]
        } 

gtkEnumUI :: [String] -> UIGTK Int
gtkEnumUI labels = UIGTK "" $ \ctx -> do
    combo <- comboBoxNewText
    forM_ labels $ \label -> comboBoxAppendText combo label
    comboBoxSetActive combo 0
    align <- alignmentNew 0 0 0 0
    containerAdd align combo

    return GTKWidget {
          ui_widget = toWidget align,
          ui_set = \vs -> (comboBoxSetActive combo vs),
          ui_get = fmap eVal (comboBoxGetActive combo),
          ui_reset = comboBoxSetActive combo 0,
          ui_packWide = False,
          ui_tableXAttach = [Expand,Shrink,Fill],
          ui_tableYAttach = []
        } 

gtkDefaultUI :: a -> UIGTK a -> UIGTK a
gtkDefaultUI a ui = UIGTK (ui_label ui) $ \ctx -> do
    gw <- ui_create ui ctx
    ui_set gw a
    return gw{ui_reset=ui_set gw a}


-- uiNew :: (UI GTK a) -> IO (GTKWidget a)
-- uiNew (UIGTK _ uia) = uia (GTKCTX CS_NORMAL (return ()))

type DialogResult a = Maybe a
type DialogButton a = (String,GTKWidget a -> IO (Maybe (DialogResult a)))

data ModalDialog a = ModalDialog {
    md_dialog :: Dialog,
    md_gw :: GTKWidget a,
    md_run :: IO (DialogResult a)
}

modalDialogNew :: String -> UIGTK a -> [DialogButton a] -> IO (ModalDialog a)
modalDialogNew title ui buttons = do
    dialog <- dialogNew
    resultv <- newIORef undefined
    set dialog [ windowTitle := title ]

    -- Not sure how to pass an activate action in the context to
    -- create a widget when it needs to reference the widget being
    -- created. Revert to mutation...
    activatefv <- newIORef (return ())
    let ctx = GTKCTX (readIORef activatefv >>= id)
    gw <- ui_create ui ctx

    let activatef = do
        ev <- ui_get gw
        case ev of
           (ErrVal (Right s)) -> return ()
           (ErrVal (Left v)) -> do
               writeIORef resultv (Just v)
               dialogResponse dialog ResponseOk
    writeIORef activatefv activatef

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

    let runDialog = do
        widgetShow dialog
        r <- dialogRun dialog
        v <- readIORef resultv
        widgetHide dialog
        return v

    return (ModalDialog dialog gw runDialog)
        
dialogOK, dialogReset, dialogCancel :: DialogButton a
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
        return (Just Nothing)

getListSelection :: TreeView -> IO (Maybe Int)
getListSelection tree = do
    sel <- treeViewGetSelection tree
    miter <- treeSelectionGetSelected sel
    case miter of
        Nothing -> return Nothing
        Just (iter) -> return (Just (listStoreIterToIndex iter))

maybeM :: (Monad m) => (Maybe a) -> (a -> m ()) -> m ()
maybeM Nothing m = return ()
maybeM (Just a) m = m a

stockButton stockId size action = do
    b <- buttonNew
    image <- imageNewFromStock stockId IconSizeSmallToolbar
    containerAdd b image
    on b buttonActivated action
    set b [buttonRelief := ReliefNone]
    return b



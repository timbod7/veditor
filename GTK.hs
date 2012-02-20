{-# LANGUAGE TypeFamilies #-}

module GTK(
    GTK,
    GTKWidget(..),
    uiNew,
    modalDialogNew,
    ModalDialog(..),
    dialogOK, dialogReset, dialogCancel,
    maybeM
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

data GTKCTX = GTKCTX {
    cs_state :: CTXState,
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
    defaultUI = gtkDefaultUI


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
        gw <- ui_create ui ctx{cs_state=CS_NORMAL}
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

        -- create the subsequent rows
        gw2 <- ui_create uis ctx{cs_state=CS_AND table (i+1)}

        return GTKWidget {
           ui_widget = toWidget table,
           ui_set = \(a :&: b) -> ui_set gw a >> ui_set gw2 b,
           ui_get = do
               a <- ui_get gw
               b <- ui_get gw2
               return ((:&:) <$> a <*> b),
           ui_reset = ui_reset gw >> ui_reset gw2,
           ui_packWide = True,
           ui_tableXAttach = xattach,
           ui_tableYAttach = yattach
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
            ui_reset = return (),
            ui_packWide = False,
            ui_tableXAttach = [],
            ui_tableYAttach = []
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

        dgw <- delayIO (ui_create ui ctx{cs_state=CS_NORMAL})

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
          ui_reset = reset,
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

gtkListUI :: (a -> String) -> UI GTK a -> UI GTK [a]
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

gtkDefaultUI :: a -> UI GTK a -> UI GTK a
gtkDefaultUI a ui = UIGTK (ui_label ui) $ \ctx -> do
    gw <- ui_create ui ctx
    return gw{ui_reset=ui_set gw a}


uiNew :: (UI GTK a) -> IO (GTKWidget a)
uiNew (UIGTK _ uia) = uia (GTKCTX CS_NORMAL (return ()))

type DialogResult a = Maybe a
type DialogButton a = (String,GTKWidget a -> IO (Maybe (DialogResult a)))

data ModalDialog a = ModalDialog {
    md_dialog :: Dialog,
    md_gw :: GTKWidget a,
    md_run :: IO (DialogResult a)
}

modalDialogNew :: String -> UI GTK a -> [DialogButton a] -> IO (ModalDialog a)
modalDialogNew title ui buttons = do
    dialog <- dialogNew
    resultv <- newIORef undefined
    set dialog [ windowTitle := title ]

    -- Not sure how to pass an activate action in the context to
    -- create a widget when it needs to reference the widget being
    -- created. Revert to mutation...
    activatefv <- newIORef (return ())
    let ctx = GTKCTX CS_NORMAL (readIORef activatefv >>= id)
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



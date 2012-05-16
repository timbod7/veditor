{-# LANGUAGE GADTs #-}

module GTK(
    GTKWidget(..),
    VEGTK(..),
    GTKCTX(..),
    ModalDialog(..),
    modalDialogNew,
    dialogOK, dialogReset, dialogCancel,
    maybeM,
    uiGTK
) where

import Graphics.UI.Gtk
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Char
import qualified Data.Map as Map

import VE
import DelayIO
import ErrVal

data GTKCTX e = GTKCTX {
    cs_onActivate :: IO (),
    cs_initialEnv :: e
}

data GTKWidget e a = GTKWidget {
    ui_widget :: Widget,
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a),
    ui_reset :: IO (),
    ui_refreshEnv :: e -> IO (),
    ui_packWide :: Bool,
    ui_tableXAttach :: [AttachOptions],
    ui_tableYAttach :: [AttachOptions]
}

data VEGTK e a = VEGTK {
    ui_label :: String,
    ui_create :: (GTKCTX e -> IO (GTKWidget e a))
}

type VEE e a = VE (IOE e) a

uiGTK  :: VE (IOE e) a -> VEGTK e a
uiGTK Entry = gtkEntry pure id
uiGTK (Label label ui) = gtkLabel label (uiGTK ui)
uiGTK (MapVE fab fba Entry) = gtkEntry fab fba
uiGTK (MapVE fab fba ui) = gtkMapVE fab fba (uiGTK ui)
uiGTK (DefaultVE a ui) = gtkDefaultVE a (uiGTK ui)
uiGTK (EnumVE ss) = gtkEnumVE ss
uiGTK (ListVE toString ui) = gtkListVE toString (uiGTK ui)
uiGTK (AndVE uia uib) = gtkAndVE uia uib
uiGTK (OrVE uia uib) = gtkOrVE uia uib
uiGTK (MaybeVE (MapVE fab fba Entry)) = gtkMaybeMapEntry fab fba
uiGTK (MaybeVE ui) = uiGTK (maybeVE_ ui)
uiGTK NullVE = gtkNull

gtkAndVE :: (VEE e a) -> (VEE e b) -> VEGTK e (a,b)
gtkAndVE uia uib = VEGTK "" $ \ctx -> do
    let ui = (AndVE uia uib)
    let nRows = countFields ui
    table <- tableNew nRows 2 False
    rowiv <- newIORef 0
    addFields table rowiv ctx ui 
  where
    countFields :: VEE e a -> Int
    countFields (AndVE uia uib) = countFields uia + countFields uib
    countFields _ = 1

    addFields :: Table -> IORef Int -> GTKCTX e -> VEE e a -> IO (GTKWidget e a)
    addFields table rowiv ctx (AndVE uia uib) = do
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
            ui_refreshEnv = \e -> ui_refreshEnv gwa e >> ui_refreshEnv gwa e,
            ui_packWide = True,
            ui_tableXAttach = [Expand,Shrink,Fill],
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
            w <- withBorder 10 (ui_widget gw)
            containerAdd f w
            tableAttach table f 0 2 i (i+1) xattach yattach 0 5
          else do
            label <- labelNew (Just (ui_label ui))
            miscSetAlignment label 0 0.5
            tableAttach table label 0 1 i (i+1) [Fill] [Fill] 5 0
            tableAttach table (ui_widget gw) 1 2 i (i+1) xattach yattach 0 0
        return gw

gtkMapVE :: (a -> ErrVal b) -> (b -> a) -> VEGTK e a -> VEGTK e b
gtkMapVE fab fba (VEGTK label uia) = VEGTK label mkui
      where
        mkui ctx = do
            uiwa <- uia ctx
            return uiwa {
              ui_set=(ui_set uiwa).fba,
              ui_get=do
                 ea <- ui_get uiwa
                 case ea of
                     (EValue a) -> return (fab a)
                     (Error m c) -> return (Error m c)
            }
         

invalidEntryBackground = Color 65535 50000 50000

gtkNull :: VEGTK e ()
gtkNull = VEGTK "" $ \cts -> do
    l <- labelNew Nothing
    return GTKWidget {
        ui_widget = toWidget l,
        ui_set = \v -> return (),
        ui_get = return (eVal ()),
        ui_reset = return (),
        ui_refreshEnv = const (return ()),
        ui_packWide = False,
        ui_tableXAttach = [],
        ui_tableYAttach = []
        } 

gtkEntry :: (String -> ErrVal a) -> (a -> String) -> VEGTK e a
gtkEntry fromString toString = VEGTK "" $ \ctx -> do
    e <- entryNew
    e `on` editableChanged $ setBackground e
    e `on` entryActivate  $ cs_onActivate ctx
    setBackground e
    return GTKWidget {
        ui_widget = toWidget e,
        ui_set = entrySetText e.toString,
        ui_get = fmap fromString (entryGetText e),
        ui_reset = entrySetText e "",
        ui_refreshEnv = const (return ()),
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

gtkLabel :: String -> VEGTK e a -> VEGTK e a
gtkLabel label ui = ui{
    ui_label=label',
    ui_create= (\ctx -> do
        gw <- ui_create ui ctx
        return gw{ui_get = do
            ev <- ui_get gw
            return (eContext label' ev)
        }
    )
    }
  where
    label' = labelString label

data UnionState e = UnionState {
    us_table :: Table,
    us_combo :: ComboBox,
    us_current :: IORef (Maybe Int),                                       -- The current enabled index
    us_active :: IORef (Map.Map Int (Widget, IO (), e -> IO ())),  -- The widgets that have been created
    us_callback :: IORef (Map.Map Int (IO ())),                    -- Callback to show widget for a given index
    us_i :: IORef Int
}

gtkOrVE :: VEE e a -> VEE e b -> VEGTK e (Either a b)
gtkOrVE uia uib = VEGTK "" $ \ctx -> do
    let ui = (OrVE uia uib)
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
    active <- newIORef Map.empty
    let ustate = UnionState table combo wref active cref rowiv 

    gw <- addChoices ustate ctx ui
    return gw { 
        ui_widget = toWidget frame
    }

  where
    getLabels :: VEE e a -> [String]
    getLabels (OrVE uia uib) = getLabels uia ++ getLabels uib
    getLabels ui = [ui_label (uiGTK ui)]

    resetActive :: UnionState e -> IO ()
    resetActive us = do
        active <- readIORef (us_active us)
        sequence_ [ reset | (i,(w,reset,refresh)) <- Map.toList active ]

    refreshActive :: UnionState e -> e -> IO ()
    refreshActive us env = do
        active <- readIORef (us_active us)
        sequence_ [ refresh env | (i,(w,reset,refresh)) <- Map.toList active ]

    addChoices :: UnionState e -> GTKCTX e -> VEE e a -> IO (GTKWidget e a)
    addChoices us ctx (OrVE uia uib) = do
        gwa <- addChoices us ctx uia
        ileft <- readIORef (us_i us)
        gwb <- addChoices us ctx uib

        let isLeftSelected = do
            mc <- readIORef (us_current us)
            case mc of
                Nothing -> error "impossible"
                (Just i) -> return (i < ileft)

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
            ui_reset = resetActive us >> comboBoxSetActive (us_combo us) 0,
            ui_refreshEnv = refreshActive us,
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

        let showThisVE = do
              let wref = us_current us
              let table = us_table us
              let attach w = tableAttach table w 0 1 0 1 [Expand,Fill] [Expand,Fill] 10 0
              gw <- delayGet dgw
              modifyIORef (us_active us) (Map.insert i (ui_widget gw,ui_reset gw,ui_refreshEnv gw))
              mw <- readIORef wref
              case mw of
                Nothing -> do
                  attach (ui_widget gw)
                  widgetShowAll (ui_widget gw)
                  writeIORef wref (Just i)
                (Just oi) | oi == i -> return ()
                          | otherwise -> do
                  (Just (w,_,_)) <- fmap (Map.lookup oi) (readIORef (us_active us))
                  containerRemove table w
                  attach (ui_widget gw)
                  widgetShowAll (ui_widget gw)
                  writeIORef wref (Just i)
              return gw

            setCombo = comboBoxSetActive (us_combo us) i

            set a = do
                setCombo
                gw <- showThisVE
                ui_set gw a

        modifyIORef (us_callback us) (Map.insert i (void showThisVE))

        when (i==0) $ do
            setCombo
            void showThisVE

        return GTKWidget {
            ui_widget = toWidget (us_table us),
            ui_set = set,
            ui_get = showThisVE >>= ui_get,
            ui_reset = return (),
            ui_refreshEnv= const (return ()),
            ui_packWide = True,
            ui_tableXAttach = [Expand,Shrink,Fill],
            ui_tableYAttach = [Expand,Shrink,Fill]
         } 

gtkListVE :: (a -> String) -> VEGTK e a -> VEGTK e [a]
gtkListVE toString ui = VEGTK "" $ \ctx -> do
    ls <- listStoreNew []
    tree <- treeViewNewWithModel ls
    col1 <- treeViewColumnNew

    env <- newIORef (cs_initialEnv ctx)
    ddialog <- delayIO $ do
        e <- readIORef env                 
        modalDialogNew e "List Edit" ui [dialogOK,dialogReset,dialogCancel]

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

    let refreshEnv e = do
        pending <- delayPending ddialog
        if pending then writeIORef env e
                   else do
                     d <- delayGet ddialog
                     ui_refreshEnv (md_gw d) e
                                            
    return GTKWidget {
          ui_widget = toWidget hbox,
          ui_set = \vs -> do
              listStoreClear ls
              mapM_ (listStoreAppend ls) vs,
          ui_get = fmap eVal (listStoreToList ls),
          ui_reset = listStoreClear ls,
          ui_refreshEnv=refreshEnv,
          ui_packWide = True,
          ui_tableXAttach = [Expand,Shrink,Fill],
          ui_tableYAttach = [Expand,Shrink,Fill]
        } 

gtkEnumVE :: IOE e [String] -> VEGTK e Int
gtkEnumVE (IOE labelsf) = VEGTK "" $ \ctx -> do
    combo <- comboBoxNewText
    align <- alignmentNew 0 0 0 0
    containerAdd align combo

    let setLabels ls = do
          comboBoxGetModelText combo >>= listStoreClear
          comboBoxAppendText combo " -select- "
          forM_ ls $ \label -> comboBoxAppendText combo label
          comboBoxSetActive combo 0

    initialLabels <- labelsf (cs_initialEnv ctx)

    setLabels initialLabels

    return GTKWidget {
          ui_widget = toWidget align,
          ui_set = \vs -> (comboBoxSetActive combo (vs+1)),
          ui_get = do
              i <- comboBoxGetActive combo
              if i == 0 then return (eErr "No input selected")
                        else return (eVal (i-1))
              ,
          ui_reset = comboBoxSetActive combo 0,
          ui_refreshEnv= \e -> labelsf e >>= setLabels,
          ui_packWide = False,
          ui_tableXAttach = [Expand,Shrink,Fill],
          ui_tableYAttach = []
        } 

gtkDefaultVE :: a -> VEGTK e a -> VEGTK  e a
gtkDefaultVE a ui = VEGTK (ui_label ui) $ \ctx -> do
    gw <- ui_create ui ctx
    ui_set gw a
    return gw{ui_reset=ui_reset gw >> ui_set gw a}

-- If we have a MaybeVE (MapVE ... Entry) then we 
-- can just use a single entry field provided that an
-- empty string can be used for the Nothing case.
gtkMaybeMapEntry :: (String -> ErrVal a) -> (a -> String) -> VEGTK e (Maybe a)
gtkMaybeMapEntry fsa fas = case fsa "" of
    (Error _ _) -> gtkEntry fsma fmas
    _ -> uiGTK (maybeVE_ (MapVE fsa fas Entry))
  where
    fsma "" = eVal Nothing
    fsma s = fmap Just (fsa s)
    fmas Nothing = ""
    fmas (Just a) = fas a

-- uiNew :: (VE GTK a) -> IO (GTKWidget a)
-- uiNew (VEGTK _ uia) = uia (GTKCTX CS_NORMAL (return ()))

type DialogResult a = Maybe a
type DialogButton e a = (String,GTKWidget e a -> IO (Maybe (DialogResult a)))

data ModalDialog e a = ModalDialog {
    md_dialog :: Dialog,
    md_gw :: GTKWidget e a,
    md_run :: IO (DialogResult a)
}

modalDialogNew :: e -> String -> VEGTK e a -> [DialogButton e a] -> IO (ModalDialog e a)
modalDialogNew e title ui buttons = do
    dialog <- dialogNew
    resultv <- newIORef Nothing
    set dialog [ windowTitle := title ]

    -- Not sure how to pass an activate action in the context to
    -- create a widget when it needs to reference the widget being
    -- created. Revert to mutation...
    activatefv <- newIORef (return ())
    let ctx = GTKCTX (readIORef activatefv >>= id) e
    gw <- ui_create ui ctx

    let activatef = do
        ev <- ui_get gw
        case ev of
           (Error{}) -> return ()
           (EValue v) -> do
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
        ui_refreshEnv gw e
        widgetShow dialog
        r <- dialogRun dialog
        v <- readIORef resultv
        widgetHide dialog
        return v

    return (ModalDialog dialog gw runDialog)
        
dialogOK, dialogReset, dialogCancel :: DialogButton e a
dialogOK = ("OK",ok)
  where
    ok gw = do
        ev <- ui_get gw
        case ev of
           (EValue v) -> return (Just (Just v))
           (Error{}) -> return Nothing

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

-- | Make a field name into a more human readable string.
-- Replace '_' with space and insert spaces into camelcase.
labelString :: String -> String
labelString [] = []
labelString (c1:c2:cs) | isLower c1 && isUpper c2 = c1 : ' ' : c2 : labelString cs
labelString (c:cs) | c == '_'   = ' ' : labelString cs
                   | otherwise  = c : labelString cs 

withBorder n w = do
    align <- alignmentNew 0 0 1 1
    containerAdd align w
    containerSetBorderWidth align n
    return align
    
    
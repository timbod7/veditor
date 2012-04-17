{-# LANGUAGE GADTs,TypeSynonymInstances #-}

module UI where

import ErrVal

data BiMap a b = BiMap {
    m_aeb :: a -> ErrVal b,
    m_ba :: b -> a
}

newtype ConstE a = ConstE a
newtype IOE e a = IOE (e -> IO a)

data UI e a where
    Entry :: BiMap String a -> UI e a
    Label :: String -> UI e a -> UI e a

    AndUI :: (UI e a) -> (UI e b) -> UI e (a,b)
    OrUI  :: (UI e a) -> (UI e b) -> UI e (Either a b)

    EnumUI :: e [String] -> UI e Int

    ListUI :: (a->String) -> UI e a -> UI e [a]

    -- | Convert a UI over a type a, to a UI over a type b, given
    -- the necessary mapping
    MapUI :: BiMap a b -> UI e a -> UI e b

    DefaultUI :: a -> UI e a -> UI e a

stringUI :: UI e String
stringUI = Entry (BiMap eVal id)

-- | A UI for any type implemented Read and Show.
readUI :: (Read a, Show a) => String -> UI e a
readUI typestr = Entry (BiMap fromString show)
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Not a valid "++typestr)

-- | A UI for an optional value of any type implemented Read and Show.
maybeReadUI :: (Read a, Show a) => String -> UI e (Maybe a)
maybeReadUI typestr = Entry (BiMap fromString toString)
  where
    fromString "" = eVal Nothing
    fromString s = case reads s of
        [(a,"")] -> eVal (Just a)
        _ -> eErr ("Not a valid "++typestr)

    toString Nothing = ""
    toString (Just a) = show a

intUI :: UI e Int
intUI = readUI "Int"

boolUI :: UI ConstE Bool
boolUI = MapUI (BiMap (eVal.toEnum) fromEnum) (EnumUI (ConstE ["False","True"]))

x :: UI ConstE Int
x = EnumUI (ConstE ["False","True"])

(.*.) = AndUI
(.+.) = OrUI

infixr 5 .*.
infixr 5 .+.

mapUI aeb ba = MapUI (BiMap aeb ba)
label = Label
defaultUI = DefaultUI
listUI = ListUI

fieldUI :: (HasUI a) => String -> UI ConstE a
fieldUI label = Label label mkUI

class HasUI a where
  mkUI :: UI ConstE a

instance HasUI Int where
  mkUI = readUI "Int"

instance HasUI String where
  mkUI = stringUI

instance HasUI Bool where
  mkUI = boolUI

ioeGet :: IOE e a -> e -> IO (ConstE a)
ioeGet (IOE fa) e = fa e >>= return.ConstE

ioFromConstUI :: UI ConstE a -> UI (IOE e) a
ioFromConstUI (Entry bm) = Entry bm
ioFromConstUI (Label s ui) = Label s (ioFromConstUI ui)
ioFromConstUI (AndUI ui1 ui2) = AndUI (ioFromConstUI ui1) (ioFromConstUI ui2)
ioFromConstUI (OrUI ui1 ui2) = OrUI (ioFromConstUI ui1) (ioFromConstUI ui2)
ioFromConstUI (EnumUI (ConstE ss)) = EnumUI (IOE (\e -> return ss))
ioFromConstUI (ListUI sf ui) = ListUI sf (ioFromConstUI ui)
ioFromConstUI (MapUI bm ui) = MapUI bm (ioFromConstUI ui)
ioFromConstUI (DefaultUI a ui) = DefaultUI a (ioFromConstUI ui)

snapshotUI :: e -> UI (IOE e) a -> IO (UI ConstE a)
snapshotUI e (Entry bm) = return (Entry bm)
snapshotUI e (Label s ui) = snapshotUI e ui >>= return.Label s
snapshotUI e (AndUI ui1 ui2) = do
    ui1' <- snapshotUI e ui1
    ui2' <- snapshotUI e ui2
    return (AndUI ui1' ui2')
snapshotUI e (OrUI ui1 ui2) = do
    ui1' <- snapshotUI e ui1
    ui2' <- snapshotUI e ui2
    return (OrUI ui1' ui2')
snapshotUI e (EnumUI ioe) = ioeGet ioe e >>= return.EnumUI
snapshotUI e (ListUI sf ui) = snapshotUI e ui >>= return.ListUI sf
snapshotUI e (MapUI bm ui) = snapshotUI e ui >>= return.MapUI bm
snapshotUI e (DefaultUI a ui) = snapshotUI e ui >>= return.DefaultUI a


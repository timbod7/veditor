{-# LANGUAGE GADTs,TypeSynonymInstances,FlexibleInstances #-}

module VE where

import ErrVal

-- | A GADT describing abstracted, composable user interfaces for manipulating
-- a value of type a. e is a type constructor specifing how VE specific values
-- may be possibly obtained from an environment.
data VE e a where
    -- | A VE String field
    Entry :: VE e String

    -- | Annotate a VE with a text label
    Label :: String -> VE e a -> VE e a

    -- | A null VE
    NullVE :: VE e ()

    -- | A "product" VE that combines values from two other VEs
    AndVE :: (VE e a) -> (VE e b) -> VE e (a,b)

    -- | A "sum" VE that captures the value from either of two other VEs
    OrVE  :: (VE e a) -> (VE e b) -> VE e (Either a b)

    -- | A VE for manipulating an enumeration. A list of label string are supplied,
    -- the VE value is the integer index of the selected label.
    EnumVE :: e [String] -> VE e Int

    -- | A VE for manipulating  a list of values. The supplied function lets the
    -- the VE display the list items to the user (eg for selection).
    ListVE :: (a->String) -> VE e a -> VE e [a]

    -- | A VE representing an optional value.
    MaybeVE :: VE e a -> VE e (Maybe a)

    -- | Convert a VE over a type a, to a VE over a type b, given
    -- the necessary mapping
    MapVE :: (a -> ErrVal b) -> (b -> a) -> VE e a -> VE e b

    -- | Annotate a VE with a default value
    DefaultVE :: a -> VE e a -> VE e a

newtype ConstE a = ConstE a
newtype IOE e a = IOE (e -> IO a)

-- | A VE for any type implemented Read and Show.
readVE :: (Read a, Show a) => String -> VE e a
readVE typestr = MapVE fromString show Entry 
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Not a valid "++typestr)

intVE :: VE e Int
intVE = readVE "Int"

boolVE :: VE ConstE Bool
boolVE = MapVE (eVal.toEnum) fromEnum (EnumVE (ConstE ["False","True"]))

x :: VE ConstE Int
x = EnumVE (ConstE ["False","True"])

(.*.) = AndVE
(.+.) = OrVE

infixr 5 .*.
infixr 5 .+.

mapVE :: (a -> ErrVal b) -> (b->a) -> VE e a -> VE e b
mapVE aeb ba = MapVE aeb ba

label :: String -> VE e a -> VE e a
label = Label

defaultVE :: a -> VE e a -> VE e a
defaultVE = DefaultVE

listVE :: (a->String) -> VE e a -> VE e [a]
listVE = ListVE

fieldVE :: (HasVE a) => String -> VE ConstE a
fieldVE label = Label label mkVE

class HasVE a where
  mkVE :: VE ConstE a

instance HasVE Int where
  mkVE = readVE "Int"

instance HasVE Double where
  mkVE = readVE "Double"

instance HasVE String where
  mkVE = Entry

instance HasVE Bool where
  mkVE = boolVE

instance (HasVE a) => HasVE (Maybe a)
  where
    mkVE = MaybeVE mkVE

ioeGet :: IOE e a -> e -> IO (ConstE a)
ioeGet (IOE fa) e = fa e >>= return.ConstE

ioFromConstVE :: VE ConstE a -> VE (IOE e) a
ioFromConstVE Entry = Entry
ioFromConstVE NullVE = NullVE
ioFromConstVE (Label s ui) = Label s (ioFromConstVE ui)
ioFromConstVE (AndVE ui1 ui2) = AndVE (ioFromConstVE ui1) (ioFromConstVE ui2)
ioFromConstVE (OrVE ui1 ui2) = OrVE (ioFromConstVE ui1) (ioFromConstVE ui2)
ioFromConstVE (EnumVE (ConstE ss)) = EnumVE (IOE (\e -> return ss))
ioFromConstVE (ListVE sf ui) = ListVE sf (ioFromConstVE ui)
ioFromConstVE (MapVE fab fba ui) = MapVE fab fba (ioFromConstVE ui)
ioFromConstVE (DefaultVE a ui) = DefaultVE a (ioFromConstVE ui)
ioFromConstVE (MaybeVE ui) = MaybeVE (ioFromConstVE ui)

snapshotVE :: e -> VE (IOE e) a -> IO (VE ConstE a)
snapshotVE e Entry = return Entry
snapshotVE e NullVE = return NullVE
snapshotVE e (Label s ui) = snapshotVE e ui >>= return.Label s
snapshotVE e (AndVE ui1 ui2) = do
    ui1' <- snapshotVE e ui1
    ui2' <- snapshotVE e ui2
    return (AndVE ui1' ui2')
snapshotVE e (OrVE ui1 ui2) = do
    ui1' <- snapshotVE e ui1
    ui2' <- snapshotVE e ui2
    return (OrVE ui1' ui2')
snapshotVE e (EnumVE ioe) = ioeGet ioe e >>= return.EnumVE
snapshotVE e (ListVE sf ui) = snapshotVE e ui >>= return.ListVE sf
snapshotVE e (MapVE fab fba ui) = snapshotVE e ui >>= return.MapVE fab fba
snapshotVE e (DefaultVE a ui) = snapshotVE e ui >>= return.DefaultVE a

-- A helper to build a maybe in terms of a sum definition. Useful in
-- the implementations of MaybeVE in concrete VE implemenations.
maybeVE_ :: VE e a -> VE e (Maybe a)
maybeVE_ ve = MapVE toMaybe fromMaybe
    (   label "Just" ve
    .+. label "Nothing" NullVE
    )
  where
    toMaybe (Left a) = eVal (Just a)
    toMaybe (Right ()) = eVal Nothing

    fromMaybe (Just a) = Left a
    fromMaybe Nothing = Right ()

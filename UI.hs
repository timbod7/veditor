{-# LANGUAGE GADTs,TypeSynonymInstances #-}

module UI where

import ErrVal

data UI a where
    Entry :: (String -> ErrVal a) -> (a -> String) -> UI a
    Label :: String -> UI a -> UI a

    AndUI :: (UI a) -> (UI b) -> UI (a,b)
    OrUI  :: (UI a) -> (UI b) -> UI (Either a b)

    EnumUI :: [String] -> UI Int

    ListUI :: (a->String) -> UI a -> UI [a]

    -- | Convert a UI over a type a, to a UI over a type b, given
    -- two conversion functions.
    MapUI :: (a -> ErrVal b) -> (b -> a) -> UI a -> UI b

    DefaultUI :: a -> UI a -> UI a

stringUI :: UI String
stringUI = Entry eVal id

-- | A UI for any type implemented Read and Show.
readUI :: (Read a, Show a) => String -> UI a
readUI typestr = Entry fromString show 
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Not a valid "++typestr)

intUI :: UI Int
intUI = readUI "Int"

boolUI :: UI Bool
boolUI = MapUI (eVal.toEnum) fromEnum (EnumUI ["False","True"])
   

(.*.) = AndUI
(.+.) = OrUI

infixr 5 .*.
infixr 5 .+.

mapUI = MapUI
label = Label
defaultUI = DefaultUI
listUI = ListUI

fieldUI :: (HasUI a) => String -> UI a
fieldUI label = Label label mkUI

class HasUI a where
  mkUI :: UI a

instance HasUI Int where
  mkUI = readUI "Int"

instance HasUI String where
  mkUI = stringUI

instance HasUI Bool where
  mkUI = boolUI


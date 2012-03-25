{-# LANGUAGE GADTs,TypeSynonymInstances #-}

module UI where

import ErrVal

data BiMap a b = BiMap {
    m_aeb :: a -> ErrVal b,
    m_ba :: b -> a
}

data UI a where
    Entry :: BiMap String a -> UI a
    Label :: String -> UI a -> UI a

    AndUI :: (UI a) -> (UI b) -> UI (a,b)
    OrUI  :: (UI a) -> (UI b) -> UI (Either a b)

    EnumUI :: [String] -> UI Int

    ListUI :: (a->String) -> UI a -> UI [a]

    -- | Convert a UI over a type a, to a UI over a type b, given
    -- the necessary mapping
    MapUI :: BiMap a b -> UI a -> UI b

    DefaultUI :: a -> UI a -> UI a

stringUI :: UI String
stringUI = Entry (BiMap eVal id)

-- | A UI for any type implemented Read and Show.
readUI :: (Read a, Show a) => String -> UI a
readUI typestr = Entry (BiMap fromString show)
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Not a valid "++typestr)

-- | A UI for an optional value of any type implemented Read and Show.
maybeReadUI :: (Read a, Show a) => String -> UI (Maybe a)
maybeReadUI typestr = Entry (BiMap fromString toString)
  where
    fromString "" = eVal Nothing
    fromString s = case reads s of
        [(a,"")] -> eVal (Just a)
        _ -> eErr ("Not a valid "++typestr)

    toString Nothing = ""
    toString (Just a) = show a

intUI :: UI Int
intUI = readUI "Int"

boolUI :: UI Bool
boolUI = MapUI (BiMap (eVal.toEnum) fromEnum) (EnumUI ["False","True"])
   

(.*.) = AndUI
(.+.) = OrUI

infixr 5 .*.
infixr 5 .+.

mapUI aeb ba = MapUI (BiMap aeb ba)
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


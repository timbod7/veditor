{-# LANGUAGE GADTs,TypeSynonymInstances #-}

module UI where

import ErrVal

data BiMap a b = BiMap {
    m_aeb :: a -> ErrVal b,
    m_ba :: b -> a
}

data UI e a where
    Entry :: BiMap String a -> UI e a
    Label :: String -> UI e a -> UI e a

    AndUI :: (UI e a) -> (UI e b) -> UI e (a,b)
    OrUI  :: (UI e a) -> (UI e b) -> UI e (Either a b)

    EnumUI :: (e -> [String]) -> UI e Int

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

boolUI :: UI e Bool
boolUI = MapUI (BiMap (eVal.toEnum) fromEnum) (EnumUI (const ["False","True"]))
   

(.*.) = AndUI
(.+.) = OrUI

infixr 5 .*.
infixr 5 .+.

mapUI aeb ba = MapUI (BiMap aeb ba)
label = Label
defaultUI = DefaultUI
listUI = ListUI

fieldUI :: (HasUI a) => String -> UI e a
fieldUI label = Label label mkUI

class HasUI a where
  mkUI :: UI e a

instance HasUI Int where
  mkUI = readUI "Int"

instance HasUI String where
  mkUI = stringUI

instance HasUI Bool where
  mkUI = boolUI


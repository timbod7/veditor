{-# LANGUAGE TypeFamilies #-}

module UI where

import ErrVal

class UITK tk where
    data UI tk :: * -> *
    data Field tk :: * -> *

    stringField :: UI tk String

    list :: UI tk a -> UI tk [a]

    struct :: a -> [Field tk a] -> UI tk a
    union :: a -> [Field tk a] -> UI tk a
    field :: String -> (a->f,f->a->a) -> UI tk f -> Field tk a

    -- | Convert a UI over a type a, to a UI over a type b, given
    -- two conversion functions.
    mapUI :: (a -> ErrVal b) -> (b -> a) -> UI tk a -> UI tk b
    
-- | A UI for any type implemented Read and Show. The underlying
-- UI is a string field.
readShowField :: (UITK tk, Read a, Show a) => UI tk a
readShowField = mapUI fromString show stringField
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Read failed")

intField :: (UITK tk) => UI tk Int
intField = readShowField


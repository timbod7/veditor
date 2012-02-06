{-# LANGUAGE TypeFamilies #-}

module UI where

import ErrVal

class UITK tk where
    data UI tk :: * -> *
    data Field tk :: * -> *
    data Case tk :: * -> *

    entry :: (String -> ErrVal a) -> (a -> String) -> UI tk a
                
    list :: UI tk a -> UI tk [a]

    struct :: a -> [Field tk a] -> UI tk a
    field :: String -> (a->f,f->a->a) -> UI tk f -> Field tk a

    union :: a -> [Case tk a] -> UI tk a
    ucase :: String -> (a->Maybe f,f->a) -> UI tk f -> Case tk a

    -- | Convert a UI over a type a, to a UI over a type b, given
    -- two conversion functions.
    mapUI :: (a -> ErrVal b) -> (b -> a) -> UI tk a -> UI tk b
    
stringEntry :: (UITK tk) => UI tk String
stringEntry = entry eVal id

-- | A UI for any type implemented Read and Show.
readEntry :: (UITK tk, Read a, Show a) => UI tk a
readEntry = entry fromString show 
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Read failed")


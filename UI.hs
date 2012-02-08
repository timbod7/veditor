{-# LANGUAGE TypeFamilies #-}

module UI where

import ErrVal


-- Product types
-- (ie for records and structure)

data HNil = HNil

data HAnd a b = a :&: b
infixr 9 :&:

class HProduct l
instance HProduct HNil
instance HProduct l => HProduct (HAnd e l)

-- Sum types
-- (ie for discriminated unions)

data HOr a b = HVal a
             | HSkp b

class HSum l
instance HSum HNil
instance HSum l => HSum (HOr e l)

class UITK tk where
    data UI tk :: * -> *

    entry :: (String -> ErrVal a) -> (a -> String) -> UI tk a
    label :: String -> UI tk a -> UI tk a

    nilUI ::  UI tk HNil             
    andUI :: (HProduct l) => UI tk a -> UI tk l -> UI tk (HAnd a l)
    orUI  :: (HSum l) => UI tk a -> UI tk l -> UI tk (HOr a l)

    listUI :: UI tk a -> UI tk [a]

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

readInt :: (UITK tk) => UI tk Int
readInt = readEntry


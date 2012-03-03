{-# LANGUAGE TypeFamilies,FlexibleInstances,MultiParamTypeClasses #-}

module UI where

import ErrVal

-- The HList type

data HNil = HNil deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read) 

class HList l
instance HList HNil
instance HList l => HList (HCons e l)

-- Sum types
-- (ie for discriminated unions)

data HOr a b = HVal a
             | HSkp b

----------------------------------------------------------------------

class FieldList tk l
  where
    type StructV tk l
    type UnionV tk l

instance (UITK tk) => FieldList tk HNil
  where
    type StructV tk HNil = HNil
    type UnionV tk HNil = HNil

instance (UITK tk,FieldList tk l, tk~tk') => FieldList tk (HCons (UI tk' a) l)
  where
    type StructV tk (HCons (UI tk' a) l) = HCons a (StructV tk l)
    type UnionV  tk (HCons (UI tk' a) l) = HOr a (UnionV tk l)

(.*.) :: (FieldList tk l) => UI tk a -> l  -> HCons (UI tk a) l
(.*.) = HCons

----------------------------------------------------------------------

class UITK tk where
    data UI tk :: * -> *

    entry :: (String -> ErrVal a) -> (a -> String) -> UI tk a
    label :: String -> UI tk a -> UI tk a

    nilUI ::  UI tk HNil

    structUI :: (FieldList tk l) => l -> (UI tk (StructV tk l))
    unionUI  :: (FieldList tk l) => l -> (UI tk (UnionV tk l))

    enumUI :: [String] -> UI tk Int

    listUI :: (a->String) -> UI tk a -> UI tk [a]

    defaultUI :: a -> UI tk a -> UI tk a
    
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

boolUI :: (UITK tk) => UI tk Bool
boolUI = mapUI (eVal.toEnum) fromEnum (enumUI ["False","True"])

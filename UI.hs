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

class FieldList ui l
  where
    type StructV l
    type UnionV l

instance FieldList ui HNil
  where
    type StructV HNil = HNil
    type UnionV HNil = HNil

instance (UI ui, FieldList ui' l, ui~ui' ) => FieldList ui' (HCons (ui a) l)
  where
    type StructV (HCons (ui a) l) = HCons a (StructV l)
    type UnionV  (HCons (ui a) l) = HOr a (UnionV l)

(.*.) :: (UI ui, FieldList ui l) => ui a -> l  -> HCons (ui a) l
(.*.) = HCons

fcons :: (UI ui, FieldList ui l) => ui a -> l  -> HCons (ui a) l
fcons = HCons

infixr 5 .*.

----------------------------------------------------------------------

class UI ui where
    entry :: (String -> ErrVal a) -> (a -> String) -> ui a
    label :: String -> ui a -> ui a

    nilUI ::  ui HNil

    structUI :: (FieldList ui l) => l -> (ui (StructV l))
    unionUI  :: (FieldList ui l) => l -> (ui (UnionV l))

    enumUI :: [String] -> ui Int

    listUI :: (a->String) -> ui a -> ui [a]

    defaultUI :: a -> ui a -> ui a
    
    -- | Convert a UI over a type a, to a UI over a type b, given
    -- two conversion functions.
    mapUI :: (a -> ErrVal b) -> (b -> a) -> ui a -> ui b

stringUI :: (UI ui) => ui String
stringUI = entry eVal id

-- | A UI for any type implemented Read and Show.
readUI :: (UI ui, Read a, Show a) => ui a
readUI = entry fromString show 
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Read failed")

intUI :: (UI ui) => ui Int
intUI = readUI

boolUI :: (UI ui) => ui Bool
boolUI = mapUI (eVal.toEnum) fromEnum (enumUI ["False","True"])
   
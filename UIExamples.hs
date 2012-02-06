{-# LANGUAGE TypeFamilies #-}

module UIExamples where

import UI
import ErrVal

data StructTest = StructTest {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

structTest :: (UITK tk) => UI tk StructTest
structTest = mapUI toStruct fromStruct
           $ andUI (label "v1" stringEntry)
           $ andUI (label "v2" readInt)
           $ andUI (label "v3" readInt)
           $ nilUI
   where
     toStruct (a :&: b :&: c :&: HNil) = eVal (StructTest a b c)
     fromStruct (StructTest a b c) = (a :&: b :&: c :&: HNil)

data StructTest2 = StructTest2 {
    t_v4 :: String,
    t_v5 :: Int,
    t_v6 :: StructTest
} deriving (Show)

structTest2 :: (UITK tk) => UI tk StructTest2
structTest2 = mapUI toStruct fromStruct
           $ andUI (label "v1" stringEntry)
           $ andUI (label "v2" readInt)
           $ andUI (label "v3" structTest)
           $ nilUI
   where
     toStruct (a :&: b :&: c :&: HNil) = eVal (StructTest2 a b c)
     fromStruct (StructTest2 a b c) = (a :&: b :&: c :&: HNil)

data UnionTest = UT_V1 String
               | UT_V2 Int
               | UT_V3 StructTest
               | UT_V4 UnionTest
    deriving (Show)

unionTest :: (UITK tk) => UI tk UnionTest
unionTest = mapUI toUnion fromUnion
          $ orUI (label "v1" stringEntry)
          $ orUI (label "v2" readInt)
          $ orUI (label "v3" structTest)
          $ orUI (label "v4" unionTest)
          $ nilUI
  where
    toUnion (HVal v) =  eVal (UT_V1 v)
    toUnion (HSkp (HVal v)) =  eVal (UT_V2 v)
    toUnion (HSkp (HSkp (HVal v))) =  eVal (UT_V3 v)
    toUnion (HSkp (HSkp (HSkp (HVal v)))) =  eVal (UT_V4 v)

    fromUnion (UT_V1 v) = HVal v
    fromUnion (UT_V2 v) = HSkp (HVal v)
    fromUnion (UT_V3 v) = HSkp (HSkp (HVal v))
    fromUnion (UT_V4 v) = (HSkp (HSkp (HSkp (HVal v))))

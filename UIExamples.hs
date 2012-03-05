{-# LANGUAGE TypeFamilies #-}

module UIExamples where

import UI
import ErrVal

data StructTest = StructTest {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

structTest :: (UI ui) => ui StructTest
structTest = mapUI toStruct fromStruct $ structUI $
    label "v1" stringUI .*.
    label "v2" intUI .*.
    label "v3" intUI .*.
    HNil
  where
    toStruct (HCons a (HCons b (HCons c HNil))) = eVal (StructTest a b c)
    fromStruct (StructTest a b c) = (HCons a (HCons b (HCons c HNil)))

data StructTest2 = StructTest2 {
    t_v4 :: String,
    t_v5 :: Int,
    t_v6 :: StructTest
} deriving (Show)

structTest2 :: (UI ui) => ui StructTest2
structTest2 = mapUI toStruct fromStruct $ structUI $
    label "v1" stringUI .*.
    label "v2" intUI .*.
    label "v3" structTest .*.
    HNil
  where
    toStruct (HCons a (HCons b (HCons c HNil))) = eVal (StructTest2 a b c)
    fromStruct (StructTest2 a b c) = (HCons a (HCons b (HCons c HNil)))

data UnionTest = UT_V1 String
               | UT_V2 Int
               | UT_V3 StructTest
               | UT_V4 UnionTest
    deriving (Show)

unionTest :: (UI ui) => ui UnionTest
unionTest = mapUI toUnion fromUnion $ unionUI $
    label "v1" stringUI .*.
    label "v2" intUI .*.
    label "v3" structTest .*.
    label "v4" unionTest .*.
    HNil
  where
    toUnion (HVal v) =  eVal (UT_V1 v)
    toUnion (HSkp (HVal v)) =  eVal (UT_V2 v)
    toUnion (HSkp (HSkp (HVal v))) =  eVal (UT_V3 v)
    toUnion (HSkp (HSkp (HSkp (HVal v)))) =  eVal (UT_V4 v)

    fromUnion (UT_V1 v) = HVal v
    fromUnion (UT_V2 v) = HSkp (HVal v)
    fromUnion (UT_V3 v) = HSkp (HSkp (HVal v))
    fromUnion (UT_V4 v) = (HSkp (HSkp (HSkp (HVal v))))

listTest ::(UI ui) => ui [StructTest]
listTest = defaultUI defv $ listUI show structTest
  where
    defv = [StructTest "southern" 4 5, StructTest "tasman" 5 6]


data StructTest3 = StructTest3 {
    t_v7 :: String,
    t_v8 :: Int,
    t_v9 :: Bool,
    t_v10 :: [StructTest]
} deriving (Show)

structTest3 :: (UI ui) => ui StructTest3
structTest3 = mapUI toStruct fromStruct $ structUI $
    label "v1" stringUI .*.
    label "v2" intUI .*.
    label "v3" boolUI .*.
    label "v4" (listUI show structTest) .*.
    HNil
   where
     toStruct (HCons a (HCons b (HCons c (HCons d HNil)))) = eVal (StructTest3 a b c d)
     fromStruct (StructTest3 a b c d) = (HCons a (HCons b (HCons c (HCons d HNil))))

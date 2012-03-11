{-# LANGUAGE TypeFamilies #-}

module UIExamples where

import UI
import ErrVal

data StructTest = StructTest {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

instance HasUI StructTest
  where
    mkUI = mapUI toStruct fromStruct
        (   fieldUI "v1"
        .*. fieldUI "v2"
        .*. fieldUI "v3"
        )
      where
        toStruct (a,(b,c)) = eVal (StructTest a b c)
        fromStruct (StructTest a b c) = (a,(b,c))

data StructTest2 = StructTest2 {
    t_v4 :: String,
    t_v5 :: Int,
    t_v6 :: StructTest
} deriving (Show)

instance HasUI StructTest2
  where
    mkUI = mapUI toStruct fromStruct
        (   fieldUI "v1"
        .*. fieldUI "v2"
        .*. fieldUI "v3"
        )
      where
        toStruct (a,(b,c)) = eVal (StructTest2 a b c)
        fromStruct (StructTest2 a b c) = (a,(b,c))

data UnionTest = UT_V1 String
               | UT_V2 Int
               | UT_V3 StructTest
               | UT_V4 UnionTest
    deriving (Show)

instance HasUI UnionTest
  where
    mkUI = mapUI toUnion fromUnion 
        (   fieldUI "v1"
        .+. fieldUI "v2"
        .+. fieldUI "v3"
        .+. fieldUI "v4"
        )
      where
        toUnion (Left v) =  eVal (UT_V1 v)
        toUnion (Right (Left v)) =  eVal (UT_V2 v)
        toUnion (Right (Right (Left v))) =  eVal (UT_V3 v)
        toUnion (Right (Right (Right v))) =  eVal (UT_V4 v)

        fromUnion (UT_V1 v) = Left v
        fromUnion (UT_V2 v) = Right (Left v)
        fromUnion (UT_V3 v) = Right (Right (Left v))
        fromUnion (UT_V4 v) = (Right (Right (Right v)))

listTest :: UI [StructTest]
listTest = defaultUI defv $ listUI show mkUI
  where
   defv = [StructTest "southern" 4 5, StructTest "tasman" 5 6]


data StructTest3 = StructTest3 {
    t_v7 :: String,
    t_v8 :: Int,
    t_v9 :: Bool,
    t_v10 :: [StructTest]
} deriving (Show)

instance HasUI StructTest3
  where
    mkUI = mapUI toStruct fromStruct
        (   fieldUI "v1"
        .*. fieldUI "v2"
        .*. fieldUI "v3"
        .*. label "v4" (listUI show mkUI)
        )
     where
       toStruct (a,(b,(c,d))) = eVal (StructTest3 a b c d)
       fromStruct (StructTest3 a b c d) = (a,(b,(c,d)))

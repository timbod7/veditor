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
        (   fieldUI "stringV"
        .*. fieldUI "intV1"
        .*. Label "intV2" (defaultUI 7 mkUI)
        )
      where
        toStruct (a,(b,c)) = eVal (StructTest a b c)
        fromStruct (StructTest a b c) = (a,(b,c))

data StructTest2 = StructTest2 {
    t_v4 :: String,
    t_v5 :: Maybe Int,
    t_v6 :: StructTest
} deriving (Show)

instance HasUI StructTest2
  where
    mkUI = mapUI toStruct fromStruct
        (   fieldUI "stringV"
        .*. Label "maybeIntV" (maybeReadUI "Int")
        .*. fieldUI "structV"
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
        (   fieldUI "stringV"
        .+. fieldUI "intV"
        .+. fieldUI "structV"
        .+. fieldUI "recUnionV"
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
        (   fieldUI "string"
        .*. fieldUI "int"
        .*. fieldUI "bool"
        .*. label "struct list" (listUI show mkUI)
        )
     where
       toStruct (a,(b,(c,d))) = eVal (StructTest3 a b c d)
       fromStruct (StructTest3 a b c d) = (a,(b,(c,d)))

data BinOp = Add | Sub | Mul | Div
    deriving (Enum)

instance HasUI BinOp
  where
    mkUI = mapUI (eVal.toEnum) fromEnum (EnumUI ["Add","Sub","Mul","Div"])

data Expr = Literal Double
          | BinOp BinOp Expr Expr
          | If Expr Expr Expr

instance HasUI Expr
  where
    mkUI = mapUI toExpr fromExpr
        (   Label "literal" (readUI "double")
        .+. Label "add" (mkUI .*. mkUI)
        .+. Label "sub" (mkUI .*. mkUI)
        .+. Label "mul" (mkUI .*. mkUI)
        .+. Label "div" (mkUI .*. mkUI)
        .+. Label "if"  (mkUI .*. mkUI .*. mkUI)
        )
      where
        toExpr (Left v) = eVal (Literal v)
        toExpr (Right (Left (e1,e2))) = eVal (BinOp Add e1 e2)
        toExpr (Right (Right (Left (e1,e2)))) = eVal (BinOp Sub e1 e2) 
        toExpr (Right (Right (Right (Left (e1,e2))))) = eVal (BinOp Mul e1 e2)
        toExpr (Right (Right (Right (Right (Left (e1,e2)))))) = eVal (BinOp Div e1 e2)
        toExpr (Right (Right (Right (Right (Right (e1,(e2,e3))))))) = eVal (If e1 e2 e3)

        fromExpr (Literal v)       = Left v
        fromExpr (BinOp Add e1 e2) = (Right . Left) (e1,e2)
        fromExpr (BinOp Sub e1 e2) = (Right . Right . Left) (e1,e2)
        fromExpr (BinOp Mul e1 e2) = (Right . Right . Right . Left) (e1,e2)
        fromExpr (BinOp Div e1 e2) = (Right . Right . Right . Right . Left) (e1,e2)
        fromExpr (If e1 e2 e3)     = (Right . Right . Right . Right . Right) (e1,(e2,e3))




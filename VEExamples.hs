{-# LANGUAGE TypeFamilies #-}

module VEExamples where

import System.FilePath
import System.Directory
import System.Posix
import Control.Monad
import Control.Applicative

import VE
import ErrVal

data StructTest = StructTest {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
} deriving (Show)

instance HasVE StructTest
  where
    mkVE = mapVE toStruct fromStruct
        (   label "stringV" mkVE
        .*. label "intV1"   mkVE
        .*. label "intV2"   (defaultVE 7 mkVE)
        )
      where
        toStruct (a,(b,c)) = eVal (StructTest a b c)
        fromStruct (StructTest a b c) = (a,(b,c))

data StructTest2 = StructTest2 {
    t_v4 :: String,
    t_v5 :: Maybe Int,
    t_v6 :: StructTest
} deriving (Show)

instance HasVE StructTest2
  where
    mkVE = mapVE toStruct fromStruct
        (   label "stringV" mkVE
        .*. label "maybeIntV" (maybeReadVE "Int")
        .*. label "structV" mkVE
        )
      where
        toStruct (a,(b,c)) = eVal (StructTest2 a b c)
        fromStruct (StructTest2 a b c) = (a,(b,c))

data UnionTest = UT_V1 String
               | UT_V2 Int
               | UT_V3 StructTest
               | UT_V4 UnionTest
    deriving (Show)

instance HasVE UnionTest
  where
    mkVE = mapVE (eVal.toUnion) fromUnion 
        (   label "stringV" mkVE
        .+. label "intV" mkVE
        .+. label "structV" mkVE
        .+. label "recUnionV" mkVE
        )
      where
        toUnion = either UT_V1
                $ either UT_V2
                $ either UT_V3 UT_V4

        fromUnion (UT_V1 v) = Left v
        fromUnion (UT_V2 v) = (Right . Left) v
        fromUnion (UT_V3 v) = (Right . Right . Left) v
        fromUnion (UT_V4 v) = (Right . Right . Right) v

listTest :: VE ConstE [StructTest]
listTest = defaultVE defv $ listVE show mkVE
  where
   defv = [StructTest "southern" 4 5, StructTest "tasman" 5 6]


data StructTest3 = StructTest3 {
    t_v7 :: String,
    t_v8 :: Int,
    t_v9 :: Bool,
    t_v10 :: [StructTest]
} deriving (Show)

instance HasVE StructTest3
  where
    mkVE = mapVE toStruct fromStruct
        (   label "string" mkVE
        .*. label "int" mkVE
        .*. label "bool" mkVE
        .*. label "struct list" (listVE show mkVE)
        )
     where
       toStruct (a,(b,(c,d))) = eVal (StructTest3 a b c d)
       fromStruct (StructTest3 a b c d) = (a,(b,(c,d)))

data BinOp = Add | Sub | Mul | Div
    deriving (Enum)

instance HasVE BinOp
  where
    mkVE = mapVE (eVal.toEnum) fromEnum (EnumVE (ConstE ["Add","Sub","Mul","Div"]))

data Expr = Literal Double
          | BinOp BinOp Expr Expr
          | If Expr Expr Expr

instance HasVE Expr
  where
    mkVE = mapVE (eVal.toExpr) fromExpr
        (   label "literal" (readVE "double")
        .+. label "add" (mkVE .*. mkVE)
        .+. label "sub" (mkVE .*. mkVE)
        .+. label "mul" (mkVE .*. mkVE)
        .+. label "div" (mkVE .*. mkVE)
        .+. label "if"  (mkVE .*. mkVE .*. mkVE)
        )
      where
        -- toExpr (Left v) = eVal (Literal v)
        -- toExpr (Right (Left (e1,e2))) = eVal (BinOp Add e1 e2)
        -- toExpr (Right (Right (Left (e1,e2)))) = eVal (BinOp Sub e1 e2) 
        -- toExpr (Right (Right (Right (Left (e1,e2))))) = eVal (BinOp Mul e1 e2)
        -- toExpr (Right (Right (Right (Right (Left (e1,e2)))))) = eVal (BinOp Div e1 e2)
        -- toExpr (Right (Right (Right (Right (Right (e1,(e2,e3))))))) = eVal (If e1 e2 e3)

        toExpr = either Literal
               $ either (\(e1,e2) -> BinOp Add e1 e2)
               $ either (\(e1,e2) -> BinOp Sub e1 e2)
               $ either (\(e1,e2) -> BinOp Mul e1 e2)
               $ either (\(e1,e2) -> BinOp Div e1 e2)
                        (\(e1,(e2,e3)) -> If e1 e2 e3)

        fromExpr (Literal v)       = Left v
        fromExpr (BinOp Add e1 e2) = (Right . Left) (e1,e2)
        fromExpr (BinOp Sub e1 e2) = (Right . Right . Left) (e1,e2)
        fromExpr (BinOp Mul e1 e2) = (Right . Right . Right . Left) (e1,e2)
        fromExpr (BinOp Div e1 e2) = (Right . Right . Right . Right . Left) (e1,e2)
        fromExpr (If e1 e2 e3)     = (Right . Right . Right . Right . Right) (e1,(e2,e3))

----------------------------------------------------------------------

type UserCode = Int
type RoleCode = Int

data EnvStruct = EnvStruct {
    user :: UserCode,
    role :: RoleCode
}

userCodeVE, roleCodeVE :: VE (IOE FilePath) Int
userCodeVE = EnumVE (IOE (dirContents "users"))
roleCodeVE = EnumVE (IOE (dirContents "roles"))

envStructVE :: VE (IOE FilePath) EnvStruct
envStructVE = mapVE toStruct fromStruct
    (  label "user" userCodeVE
    .*. label "role" roleCodeVE
    )
      where
        toStruct (a,b) = eVal (EnvStruct a b)
        fromStruct (EnvStruct a b) = (a,b)

dirContents :: String -> FilePath -> IO [String]
dirContents subdir root = do
       putStrLn ("Reading enums from " ++ dir)
       dirExists <- fileExist dir
       if dirExists then filterM isRegFile =<< getDirectoryContents dir
                    else return []
   where
     isRegFile fp = fmap isRegularFile (getFileStatus (combine dir fp))
     dir = combine root subdir

module Talk.Example4 where

import VE
import ErrVal
import Test

data Expr = Literal Double
          | Ref String
          | BinOp BinOp Expr Expr
          | If Expr Expr Expr

data BinOp = Add | Sub | Mul | Div
    deriving (Enum)

instance HasVE Expr
  where
    mkVE = mapVE (eVal.toExpr) fromExpr
        (   label "literal" (readVE "double")
        .+. label "ref" referenceVE
        .+. label "add" (mkVE .*. mkVE)
        .+. label "sub" (mkVE .*. mkVE)
        .+. label "mul" (mkVE .*. mkVE)
        .+. label "div" (mkVE .*. mkVE)
        .+. label "if"  (mkVE .*. mkVE .*. mkVE)
        )
      where
        toExpr = either Literal
               $ either Ref
               $ either (\(e1,e2) -> BinOp Add e1 e2)
               $ either (\(e1,e2) -> BinOp Sub e1 e2)
               $ either (\(e1,e2) -> BinOp Mul e1 e2)
               $ either (\(e1,e2) -> BinOp Div e1 e2)
                        (\(e1,(e2,e3)) -> If e1 e2 e3)

        fromExpr (Literal v)       = Left v
        fromExpr (Ref v)           = (Right . Left) v
        fromExpr (BinOp Add e1 e2) = (Right . Right . Left) (e1,e2)
        fromExpr (BinOp Sub e1 e2) = (Right . Right . Right . Left) (e1,e2)
        fromExpr (BinOp Mul e1 e2) = (Right . Right . Right . Right . Left) (e1,e2)
        fromExpr (BinOp Div e1 e2) = (Right . Right . Right . Right . Right . Left) (e1,e2)
        fromExpr (If e1 e2 e3)     = (Right . Right . Right . Right . Right . Right) (e1,(e2,e3))

instance HasVE BinOp
  where
    mkVE = mapVE (eVal.toEnum) fromEnum (EnumVE (ConstE ["Add","Sub","Mul","Div"]))

referenceVE :: VE ConstE String
referenceVE = mapVE fromVE id Entry
  where
    fromVE s | s == "" = eErr "Empty reference"
             | ' ' `elem` s || '\t' `elem` s = eErr "Whitespace in reference"
             | otherwise = eVal s

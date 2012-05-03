module Talk.Example4 where

import UI
import ErrVal
import Test

data Expr = Literal Double
          | Ref String
          | BinOp BinOp Expr Expr
          | If Expr Expr Expr

data BinOp = Add | Sub | Mul | Div
    deriving (Enum)

instance HasUI Expr
  where
    mkUI = mapUI (eVal.toExpr) fromExpr
        (   label "literal" (readUI "double")
        .+. label "ref" referenceUI
        .+. label "add" (mkUI .*. mkUI)
        .+. label "sub" (mkUI .*. mkUI)
        .+. label "mul" (mkUI .*. mkUI)
        .+. label "div" (mkUI .*. mkUI)
        .+. label "if"  (mkUI .*. mkUI .*. mkUI)
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

instance HasUI BinOp
  where
    mkUI = mapUI (eVal.toEnum) fromEnum (EnumUI (ConstE ["Add","Sub","Mul","Div"]))

referenceUI :: UI ConstE String
referenceUI = mapUI fromUI id Entry
  where
    fromUI s | s == "" = eErr "Empty reference"
             | ' ' `elem` s || '\t' `elem` s = eErr "Whitespace in reference"
             | otherwise = eVal s

module Examples.Example6 where

import Examples.Utils(testC)

import Graphics.UI.VE
import ErrVal

data Variant = VString String
             | VNumber Double
             | VArray [Variant]
             | VNUll
   deriving (Show)

instance HasVE Variant
  where
    mkVE = mapVE (eVal.toVariant) fromVariant
        (   label "string" mkVE
        .+. label "number" mkVE
        .+. label "array" (ListVE show mkVE)
        .+. label "null" NullVE
        )
      where
        toVariant :: Either String (Either Double (Either [Variant] ())) -> Variant
        toVariant = either VString
                  $ either VNumber
                  $ either VArray
                           (const VNUll)
                    
        fromVariant :: Variant -> Either String (Either Double (Either [Variant] ())) 
        fromVariant (VString v) = Left v
        fromVariant (VNumber v) = Right (Left v)
        fromVariant (VArray v) = Right (Right (Left v))
        fromVariant VNUll = Right (Right (Right ()))

test = testC (mkVE :: VE ConstE Variant)
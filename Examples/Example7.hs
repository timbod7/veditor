module Examples.Example7 where

import VE
import ErrVal
import Examples.Utils(testC)
import Examples.Example2(Person(..))

data MaybeVariants = MaybeVariants {
    st_mstring :: Maybe String,
    st_mint :: Maybe Int,
    st_mperson :: Maybe Person
} deriving (Show)

instance HasVE MaybeVariants
  where
    mkVE = mapVE toStruct fromStruct
        (   label "maybeString" mkVE
        .*. label "maybeInt"   mkVE
        .*. label "maybePerson"   mkVE
        )
      where
        toStruct (a,(b,c)) = eVal (MaybeVariants a b c)
        fromStruct (MaybeVariants a b c) = (a,(b,c))


test = testC (mkVE :: VE ConstE MaybeVariants)
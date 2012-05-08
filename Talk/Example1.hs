module Talk.Example1 where

import VE
import ErrVal
import Test

data Person = Person {
    st_name :: String,
    st_age :: Int
} deriving (Show)

instance HasVE Person
  where
    mkVE = mapVE toStruct fromStruct
        (   label "Name" mkVE
        .*. label "Age"   mkVE
        )
      where
        toStruct (a,b) = eVal (Person a b)
        fromStruct (Person a b) = (a,b)


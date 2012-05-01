module Talk.Example1 where

import UI
import ErrVal
import Test

data Person = Person {
    st_name :: String,
    st_age :: Int
} deriving (Show)

instance HasUI Person
  where
    mkUI = mapUI toStruct fromStruct
        (   label "Name" mkUI
        .*. label "Age"   mkUI
        )
      where
        toStruct (a,b) = eVal (Person a b)
        fromStruct (Person a b) = (a,b)


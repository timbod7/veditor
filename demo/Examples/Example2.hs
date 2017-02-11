module Examples.Example2 where

import Graphics.UI.VE
import ErrVal
import Examples.Utils(testC)

data Gender = Male | Female
  deriving (Show,Enum)

data Person = Person {
    st_name :: String,
    st_age :: Int,
    st_gender :: Gender
} deriving (Show)

instance HasVE Person
  where
    mkVE = mapVE toStruct fromStruct
        (   label "Name" nonEmptyString
        .*. label "Age"   mkVE
        .*. label "Gender"   mkVE
        )
      where
        toStruct (a,(b,c)) = eVal (Person a b c)
        fromStruct (Person a b c) = (a,(b,c))

nonEmptyString :: VE ConstE String
nonEmptyString = mapVE fromVE id Entry
  where
    fromVE s | s == "" = eErr "Empty String"
             | otherwise = eVal s

instance HasVE Gender
  where
    mkVE = mapVE (eVal.toEnum) fromEnum (EnumVE (ConstE ["Male","Female"]))

test = testC (mkVE :: VE ConstE Person)
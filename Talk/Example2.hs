module Talk.Example2 where

import UI
import ErrVal
import Test

data Gender = Male | Female
  deriving (Show,Enum)

data Person = Person {
    st_name :: String,
    st_age :: Int,
    st_gender :: Gender
} deriving (Show)

instance HasUI Person
  where
    mkUI = mapUI toStruct fromStruct
        (   label "Name" nonEmptyString
        .*. label "Age"   mkUI
        .*. label "Gender"   mkUI
        )
      where
        toStruct (a,(b,c)) = eVal (Person a b c)
        fromStruct (Person a b c) = (a,(b,c))

nonEmptyString :: UI ConstE String
nonEmptyString = Entry (BiMap fromUI id)
  where
    fromUI s | s == "" = eErr "Empty String"
             | otherwise = eVal s

instance HasUI Gender
  where
    mkUI = mapUI (eVal.toEnum) fromEnum (EnumUI (ConstE ["Male","Female"]))


{-# LANGUAGE TypeFamilies #-}

module Test where

import Data.Accessor
import ErrVal

data UIWidget tk a = UIWidget {
    ui_widget :: (UIW tk),
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a)
}

type UI tk a = (UICC tk) -> IO (UIWidget tk a)

class UITK tk where
    data UIW tk :: *
    data UICC tk :: *
    data Field tk :: * -> *

    stringField :: UI tk String
    struct :: a -> [Field tk a] -> UI tk a
    union :: a -> [Field tk a] -> UI tk a
    field :: String -> Accessor a f -> UI tk f -> Field tk a

stringBasedField :: (UITK tk) => (String -> ErrVal a) -> (a -> String) -> UI tk a
stringBasedField fromString toString ctx = do
    uiString <- stringField ctx
    return uiString {
        ui_set=(ui_set uiString).toString,
        ui_get=fmap (errval fromString eErr) (ui_get uiString)
    }

intField :: (UITK tk) => UI tk Int
intField = undefined

data Test = Test {
    t_v1_ :: String,
    t_v2_ :: Int,
    t_v3_ :: Int
}

t_v1 :: Accessor Test String
t_v2, t_v3 :: Accessor Test Int
t_v1 = undefined
t_v2 = undefined
t_v3 = undefined

ui :: (UITK tk) => UI tk Test
ui = struct (Test "" 0 0) [
      field "v1" t_v1 stringField,
      field "v2" t_v2 intField,
      field "v3" t_v2 intField
    ]

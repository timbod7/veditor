{-# LANGUAGE TypeFamilies #-}

module UI where

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

    list :: UI tk a -> UI tk [a]

    struct :: a -> [Field tk a] -> UI tk a
    union :: a -> [Field tk a] -> UI tk a
    field :: String -> (a->f,f->a->a) -> UI tk f -> Field tk a
    
-- | Convert a UI over a type a, to a UI over a type b, given
-- two conversion functions.
mapUI :: (UITK tk) => (a -> ErrVal b) -> (b -> a) -> UI tk a -> UI tk b
mapUI fab fba uia ctx = do
    uiwa <- uia ctx
    return uiwa {
        ui_set=(ui_set uiwa).fba,
        ui_get=fmap (errval fab eErr) (ui_get uiwa)
    }

-- | A UI for any type implemented Read and Show. The underlying
-- UI is a string field.
readShowField :: (UITK tk, Read a, Show a) => UI tk a
readShowField = mapUI fromString show stringField
  where
    fromString s = case reads s of
        [(a,"")] -> eVal a
        _ -> eErr ("Read failed")

intField :: (UITK tk) => UI tk Int
intField = readShowField

data Test = Test {
    t_v1 :: String,
    t_v2 :: Int,
    t_v3 :: Int
}

ui :: (UITK tk) => UI tk Test
ui = struct (Test "" 0 0) [
      field "v1" (t_v1,\v a->a{t_v1=v}) stringField,
      field "v2" (t_v2,\v a->a{t_v2=v}) intField,
      field "v3" (t_v3,\v a->a{t_v3=v}) intField
    ]

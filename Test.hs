module Test where

import Data.Accessor

type ErrVal = Either String

data Widget = Widget

data UIWidget a = UIWidget {
    ui_widget :: Widget,
    ui_set :: a -> IO (),
    ui_get :: IO (ErrVal a)
}

type UI a = Widget -> IO (UIWidget a)

stringField :: UI String
stringField = undefined

stringBasedField :: (String -> ErrVal a) -> (a -> String) -> UI a
stringBasedField fromString toString ctx = do
    uiString <- stringField ctx
    return uiString {
        ui_set=(ui_set uiString).toString,
        ui_get=do
           s <- ui_get uiString
           case s of
               (Left s) -> return (Left s)
               (Right s) -> return (fromString s)
    }

intField :: UI Int
intField = undefined

data Record a = Record

data RecordField a f = RecordField {
    rt_name :: String,
    rf_access :: Accessor a f,
    rf_ui :: UI f
    }

record :: a -> Record a
record = undefined

field :: Record a -> RecordField a f -> Record a
field = undefined

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


ui = record (Test "" 0 0)
     `field`  (RecordField "v1" t_v1 stringField)
     `field`  (RecordField "v2" t_v2 intField)
     `field`  (RecordField "v3" t_v2 intField)

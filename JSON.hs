{-# LANGUAGE TypeFamilies #-}

module JSON(
    JSON
) where

import Control.Applicative

import UI
import ErrVal
import qualified Data.Aeson as DA
import qualified Data.Text as Text

data JSON = JSON

instance UITK JSON where

    data UI JSON a = UIJSON {
        uj_label :: String,
        uj_tojson ::  a -> DA.Value,
        uj_fromjson :: DA.Value -> ErrVal a
    }

    entry = jsonEntry
    label s ui = ui{uj_label=s}
    nilUI = jsonNilUI 
    andUI = jsonAndUI
    orUI = jsonOrUI
    listUI = jsonListUI
    enumUI = jsonEnumUI
    mapUI = jsonMapUI
    defaultUI = jsonDefaultUI

jsonEntry :: (String -> ErrVal a) -> (a -> String) -> UI JSON a
jsonEntry fromString toString = UIJSON {
              uj_label="",
              uj_tojson = tojson,
              uj_fromjson =  fromjson
            }
  where
    tojson s = DA.String (Text.pack (toString s))
    fromjson (DA.String t) = fromString (Text.unpack t)
    fromjson _ = eErr "Non string json value found"

jsonMapUI ::  (a -> ErrVal b) -> (b -> a) -> UI JSON a -> UI JSON b
jsonMapUI fab fba uj = UIJSON {
    uj_label= uj_label uj,
    uj_tojson = uj_tojson uj.fba,
    uj_fromjson = \j -> uj_fromjson uj j >>= fab
}

jsonAndUI :: (HProduct l) => UI JSON a -> UI JSON l -> UI JSON (HAnd a l)
jsonAndUI ui uis =  UIJSON {
    uj_label="",
    uj_tojson = tojson,
    uj_fromjson =  fromjson
    }
  where
    tojson = undefined
    fromjson = undefined

jsonNilUI = undefined
jsonOrUI = undefined
jsonListUI = undefined
jsonEnumUI = undefined
jsonDefaultUI = undefined

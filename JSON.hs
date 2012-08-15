{-# LANGUAGE GADTs #-}

module JSON(
    VEJSON(..),
    uiJSON,
    jsonFromString
) where

import Control.Applicative

import VE
import ErrVal
import qualified Data.Aeson as DA
import qualified Data.Vector as DV
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.HashMap.Strict as HMap
import qualified Data.Attoparsec.ByteString as A

data VEJSON a = VEJSON {
        uj_label :: Text.Text,
        uj_default :: Maybe a,
        uj_tojson ::  a -> DA.Value,
        uj_fromjson :: DA.Value -> ErrVal a
    }

uiJSON :: VE ConstE a -> VEJSON a
uiJSON ui = mkJSON (addLabels ui)

mkJSON :: VE ConstE a -> VEJSON a
mkJSON Entry = jsonEntry
mkJSON NullVE = jsonNull
mkJSON (Label label ui) = (mkJSON ui){uj_label=Text.pack label}
mkJSON (MapVE fab fba ui) = jsonMapVE fab fba (mkJSON ui)
mkJSON (DefaultVE a ui) = (mkJSON ui){uj_default=Just a}
mkJSON (EnumVE (ConstE ss)) = jsonEnumVE ss
mkJSON (ListVE _ ui) = jsonListVE (mkJSON ui)
mkJSON (AndVE uia uib) = jsonAndVE uia uib
mkJSON (OrVE uia uib) = jsonOrVE uia uib
mkJSON (MaybeVE ui) = jsonMaybeVE ui

-- | Recursively add numeric labels to and/or branches
-- which don't already have them
addLabels :: VE ConstE a -> VE ConstE a
addLabels (Label label ui) = Label label (addLabels ui)
addLabels (MapVE fab fba ui) = MapVE fab fba (addLabels ui)
addLabels (DefaultVE a ui) = DefaultVE a (addLabels ui)
addLabels (ListVE sf ui) = ListVE sf (addLabels ui)
addLabels (MaybeVE ui) = MaybeVE (addLabels ui)
addLabels ui@(AndVE uia uib) = fst (addAndLabels ui 0)
addLabels ui@(OrVE uia uib) = fst (addOrLabels ui 0)
addLabels ui = ui

addAndLabels :: VE ConstE a -> Int -> (VE ConstE a,Int)
addAndLabels (AndVE uia uib) i0 = 
     let (uia',i1) = addAndLabels uia i0
         (uib',i2) = addAndLabels uib i1
     in (AndVE uia' uib',i2)
addAndLabels ui@(Label _ _) i = (addLabels ui,i)
addAndLabels ui i = (Label ("_"++show i) (addLabels ui),i+1)

addOrLabels :: VE ConstE a -> Int -> (VE ConstE a,Int)
addOrLabels (OrVE uia uib) i0 = 
    let (uia',i1) = addOrLabels uia i0
        (uib',i2) = addOrLabels uib i1
    in (OrVE uia' uib',i2)
addOrLabels ui@(Label _ _) i = (addLabels ui,i)
addOrLabels ui i = (Label ("_"++show i) (addLabels ui),i+1)

jsonEntry :: VEJSON String
jsonEntry = VEJSON {
              uj_label=Text.empty,
              uj_default=Nothing,
              uj_tojson = tojson,
              uj_fromjson =  fromjson
            }
  where
    tojson s = DA.String (Text.pack s)
    fromjson (DA.String t) = pure (Text.unpack t)
    fromjson _ = eErr "Non string json value found"

jsonNull :: VEJSON ()
jsonNull = VEJSON {
              uj_label=Text.empty,
              uj_default=Nothing,
              uj_tojson = const DA.Null,
              uj_fromjson =  const (pure ())
            }

jsonMapVE ::  (a -> ErrVal b) -> (b -> a) -> VEJSON a -> VEJSON b
jsonMapVE fab fba uj = VEJSON {
    uj_label= uj_label uj,
    uj_default=Nothing,
    uj_tojson = uj_tojson uj.fba,
    uj_fromjson = \j -> uj_fromjson uj j >>= fab
}

jsonListVE :: VEJSON a -> VEJSON [a]
jsonListVE uj = VEJSON {
    uj_label= uj_label uj,
    uj_default=Nothing,
    uj_tojson = \as -> DA.Array (DV.fromList (map (uj_tojson uj) as)),
    uj_fromjson = \j -> case j of
        (DA.Array v) -> sequence (map (uj_fromjson uj) (DV.toList v))
        _ -> eErr "expected a json array"
}

jsonAndVE :: VE ConstE a -> VE ConstE b -> VEJSON (a,b)
jsonAndVE uia uib = VEJSON {
    uj_label= Text.empty,
    uj_default=Nothing,
    uj_tojson = DA.Object . tojson ui,
    uj_fromjson = \j -> case j of
        (DA.Object o) -> fromjson o ui
        _ -> eErr "expected a json object"
    }
  where
    ui = AndVE uia uib

    tojson :: VE ConstE a -> a -> DA.Object
    tojson (AndVE uia uib) (a,b)= tojson uia a `HMap.union` tojson uib b
    tojson ui a =
        let uj = mkJSON ui
        in HMap.singleton (uj_label uj) (uj_tojson uj a)

    fromjson :: DA.Object -> VE ConstE a -> ErrVal a
    fromjson o (AndVE uia uib) = (,) <$> fromjson o uia <*> fromjson o uib
    fromjson o ui = 
        let uj = mkJSON ui
        in case HMap.lookup (uj_label uj) o of
            (Just j) -> uj_fromjson uj j
            Nothing -> case uj_default uj of
                 Nothing -> eErr ("field " ++ Text.unpack (uj_label uj) ++ " missing")
                 (Just v) -> eVal v

jsonOrVE :: VE ConstE a -> VE ConstE b -> VEJSON (Either a b)
jsonOrVE uia uib = VEJSON {
    uj_label= Text.empty,
    uj_default=Nothing,
    uj_tojson = DA.Object . tojson ui,
    uj_fromjson = \j -> case j of
        (DA.Object o) -> fromjson o ui
        _ -> eErr "expected a json object"
    }
  where
    ui = OrVE uia uib

    tojson :: VE ConstE a -> a -> DA.Object
    tojson (OrVE uia uib) (Left a) = tojson uia a
    tojson (OrVE uia uib) (Right b) = tojson uib b
    tojson ui a = 
        let uj = mkJSON ui
        in HMap.singleton (uj_label uj) (uj_tojson uj a)

    fromjson :: DA.Object -> VE ConstE a -> ErrVal a
    fromjson o ui = case HMap.toList o of
        [(k,v)] -> case fromjson1 k v ui of
            Nothing -> eErr ("Illegal union key " ++ Text.unpack k)
            (Just v) -> v
        _ -> eErr "Union must be a single key value pair"

    fromjson1 :: Text.Text -> DA.Value -> VE ConstE a -> (Maybe (ErrVal a))
    fromjson1 l v (OrVE uia uib) = case fromjson1 l v uia of
        Nothing -> case fromjson1 l v uib of
            Nothing -> Nothing
            (Just ev) -> (Just (fmap Right ev))
        (Just ev) -> (Just (fmap Left ev))
    fromjson1 l v ui | l == (uj_label uj) = (Just (uj_fromjson uj v))
                     | otherwise = Nothing
      where
        uj = mkJSON ui

jsonEnumVE :: [String] -> VEJSON Int
jsonEnumVE ss = VEJSON {
    uj_label= Text.empty,
    uj_default=Nothing,
    uj_tojson= tojson,
    uj_fromjson=fromjson
  }
  where
    labels = map Text.pack ss
    intToLabel = HMap.fromList (zip [0..] labels)
    labelToInt = HMap.fromList (zip labels [0..])

    tojson i = case HMap.lookup i intToLabel of
        Nothing -> error "Illegal index for enumeration"
        (Just l) -> DA.String l

    fromjson (DA.String t) = case HMap.lookup t labelToInt of
        Nothing -> eErr ("Illegal enumeration label " ++ Text.unpack t)
        (Just i) -> eVal i
    fromjson _ = eErr "Non string json value found"

jsonMaybeVE :: VE ConstE a -> VEJSON (Maybe a)
jsonMaybeVE  ui = mkJSON (maybeVE_ ui)


jsonFromString :: String -> Either String DA.Value
jsonFromString s =  A.parseOnly (DA.json) (Text.encodeUtf8 (Text.pack s))
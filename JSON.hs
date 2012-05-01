{-# LANGUAGE GADTs #-}

module JSON(
    UIJSON(..),
    uiJSON
) where

import Control.Applicative

import UI
import ErrVal
import qualified Data.Aeson as DA
import qualified Data.Vector as DV
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HMap

data UIJSON a = UIJSON {
        uj_label :: Text.Text,
        uj_default :: Maybe a,
        uj_tojson ::  a -> DA.Value,
        uj_fromjson :: DA.Value -> ErrVal a
    }

uiJSON :: UI ConstE a -> UIJSON a
uiJSON ui = mkJSON (addLabels ui)

mkJSON :: UI ConstE a -> UIJSON a
mkJSON Entry = jsonEntry
mkJSON (Label label ui) = (mkJSON ui){uj_label=Text.pack label}
mkJSON (MapUI (BiMap fab fba) ui) = jsonMapUI fab fba (mkJSON ui)
mkJSON (DefaultUI a ui) = (mkJSON ui){uj_default=Just a}
mkJSON (EnumUI (ConstE ss)) = jsonEnumUI ss
mkJSON (ListUI _ ui) = jsonListUI (mkJSON ui)
mkJSON (AndUI uia uib) = jsonAndUI uia uib
mkJSON (OrUI uia uib) = jsonOrUI uia uib

-- | Recursively add numeric labels to and/or branches
-- which don't already have them
addLabels :: UI ConstE a -> UI ConstE a
addLabels (Label label ui) = Label label (addLabels ui)
addLabels (MapUI bm ui) = MapUI bm (addLabels ui)
addLabels (DefaultUI a ui) = DefaultUI a (addLabels ui)
addLabels (ListUI sf ui) = ListUI sf (addLabels ui)
addLabels ui@(AndUI uia uib) = fst (addAndLabels ui 0)
addLabels ui@(OrUI uia uib) = fst (addOrLabels ui 0)
addLabels ui = ui

addAndLabels :: UI ConstE a -> Int -> (UI ConstE a,Int)
addAndLabels (AndUI uia uib) i0 = 
     let (uia',i1) = addAndLabels uia i0
         (uib',i2) = addAndLabels uib i1
     in (AndUI uia' uib',i2)
addAndLabels ui@(Label _ _) i = (addLabels ui,i)
addAndLabels ui i = (Label ("_"++show i) (addLabels ui),i+1)

addOrLabels :: UI ConstE a -> Int -> (UI ConstE a,Int)
addOrLabels (OrUI uia uib) i0 = 
    let (uia',i1) = addOrLabels uia i0
        (uib',i2) = addOrLabels uib i1
    in (OrUI uia' uib',i2)
addOrLabels ui@(Label _ _) i = (addLabels ui,i)
addOrLabels ui i = (Label ("_"++show i) (addLabels ui),i+1)

jsonEntry :: UIJSON String
jsonEntry = UIJSON {
              uj_label=Text.empty,
              uj_default=Nothing,
              uj_tojson = tojson,
              uj_fromjson =  fromjson
            }
  where
    tojson s = DA.String (Text.pack s)
    fromjson (DA.String t) = pure (Text.unpack t)
    fromjson _ = eErr "Non string json value found"

jsonMapUI ::  (a -> ErrVal b) -> (b -> a) -> UIJSON a -> UIJSON b
jsonMapUI fab fba uj = UIJSON {
    uj_label= uj_label uj,
    uj_default=Nothing,
    uj_tojson = uj_tojson uj.fba,
    uj_fromjson = \j -> uj_fromjson uj j >>= fab
}

jsonListUI :: UIJSON a -> UIJSON [a]
jsonListUI uj = UIJSON {
    uj_label= uj_label uj,
    uj_default=Nothing,
    uj_tojson = \as -> DA.Array (DV.fromList (map (uj_tojson uj) as)),
    uj_fromjson = \j -> case j of
        (DA.Array v) -> sequence (map (uj_fromjson uj) (DV.toList v))
        _ -> eErr "expected a json array"
}

jsonAndUI :: UI ConstE a -> UI ConstE b -> UIJSON (a,b)
jsonAndUI uia uib = UIJSON {
    uj_label= Text.empty,
    uj_default=Nothing,
    uj_tojson = DA.Object . tojson ui,
    uj_fromjson = \j -> case j of
        (DA.Object o) -> fromjson o ui
        _ -> eErr "expected a json object"
    }
  where
    ui = AndUI uia uib

    tojson :: UI ConstE a -> a -> DA.Object
    tojson (AndUI uia uib) (a,b)= tojson uia a `HMap.union` tojson uib b
    tojson ui a =
        let uj = mkJSON ui
        in HMap.singleton (uj_label uj) (uj_tojson uj a)

    fromjson :: DA.Object -> UI ConstE a -> ErrVal a
    fromjson o (AndUI uia uib) = (,) <$> fromjson o uia <*> fromjson o uib
    fromjson o ui = 
        let uj = mkJSON ui
        in case HMap.lookup (uj_label uj) o of
            (Just j) -> uj_fromjson uj j
            Nothing -> case uj_default uj of
                 Nothing -> eErr ("field " ++ Text.unpack (uj_label uj) ++ " missing")
                 (Just v) -> eVal v

jsonOrUI uia uib = UIJSON {
    uj_label= Text.empty,
    uj_default=Nothing,
    uj_tojson = DA.Object . tojson ui,
    uj_fromjson = \j -> case j of
        (DA.Object o) -> fromjson o ui
        _ -> eErr "expected a json object"
    }
  where
    ui = OrUI uia uib

    tojson :: UI ConstE a -> a -> DA.Object
    tojson (OrUI uia uib) (Left a) = tojson uia a
    tojson (OrUI uia uib) (Right b) = tojson uib b
    tojson ui a = 
        let uj = mkJSON ui
        in HMap.singleton (uj_label uj) (uj_tojson uj a)

    fromjson :: DA.Object -> UI ConstE a -> ErrVal a
    fromjson o ui = case HMap.toList o of
        [(k,v)] -> case fromjson1 k v ui of
            Nothing -> eErr ("Illegal union key " ++ Text.unpack k)
            (Just v) -> v
        _ -> eErr "Union must be a single key value pair"

    fromjson1 :: Text.Text -> DA.Value -> UI ConstE a -> (Maybe (ErrVal a))
    fromjson1 l v (OrUI uia uib) = case fromjson1 l v uia of
        Nothing -> case fromjson1 l v uib of
            Nothing -> Nothing
            (Just ev) -> (Just (fmap Right ev))
        (Just ev) -> (Just (fmap Left ev))
    fromjson1 l v ui | l == (uj_label uj) = (Just (uj_fromjson uj v))
                     | otherwise = Nothing
      where
        uj = mkJSON ui

jsonEnumUI :: [String] -> UIJSON Int
jsonEnumUI ss = UIJSON {
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



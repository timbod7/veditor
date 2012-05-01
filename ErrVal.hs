module ErrVal where

-- ErrVal captures a value, or an error state indicating
-- that the value could not be computed. The error state
-- includes a "reason" message and context information on
-- where the error occurred.
-- 
-- Instances of Functor, Applicative, Monad, Num, and Fractional are
-- provided.

import Control.Monad
import Control.Applicative

data ErrVal a = EValue a
              | Error { ereason :: String, econtext :: [String] }
    deriving (Eq,Ord,Show)

instance Functor ErrVal where
    fmap f (EValue a) = EValue (f a)
    fmap _ (Error e c) = Error e c

instance Applicative ErrVal where
    pure = EValue

    (EValue f) <*> (EValue a)  = EValue (f a)
    (Error e c) <*> _ = Error e c
    _ <*> (Error e c) = Error e c

instance Monad ErrVal where
    return  = pure

    (EValue a) >>= f = f a
    (Error e c ) >>=  f = Error e c

eVal a = EValue a
eErr s = Error s []

eContext :: String -> ErrVal a -> ErrVal a
eContext c ev@(EValue v) = ev
eContext c (Error msg cs) = Error msg (c:cs)

evMaybe :: Maybe a -> String -> ErrVal a
evMaybe (Just v) _ = eVal v
evMaybe _ s = eErr s

evFilter :: [ErrVal a] -> [a]
evFilter = foldr f []
  where
    f (EValue a) as = a:as
    f _ as= as

evlift1 :: (a->a) -> ErrVal a -> ErrVal a
evlift1 f e = pure f <*> e

evlift2 :: (a->a->a) -> ErrVal a -> ErrVal a -> ErrVal a
evlift2 f e1 e2 = pure f <*> e1 <*> e2

instance Num a => Num (ErrVal a) where
    (+) = evlift2 (+)
    (-) = evlift2 (-)
    (*) = evlift2 (*)
    negate = evlift1 negate
    abs = evlift1 abs
    signum = evlift1 signum
    fromInteger i = eVal (fromInteger i)

instance Fractional a => Fractional (ErrVal a) where
    fromRational r  = eVal (fromRational r)
    (/) = evlift2 (/)

errval :: (a -> b) -> (String -> b) -> (ErrVal a) -> b
errval fa fe (EValue a) = fa a
errval fa fe (Error e c) = fe e


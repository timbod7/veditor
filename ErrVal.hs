module ErrVal where

-- ErrVal uses the Either type to storage and propagage
-- an error message. Instances of Num and Fractional
-- are included so that maths can be done on ErrVal, without
-- requiring monadic code. We supply the monad also.

import Control.Monad
import Control.Applicative

newtype ErrVal a = ErrVal (Either a String)
    deriving (Eq,Ord,Show)

eVal v = ErrVal (Left v)
eErr s = ErrVal (Right s)

instance Monad ErrVal where
    return v  = ErrVal (Left v)
    (>>=) (ErrVal (Left a)) f = f a
    (>>=) (ErrVal (Right s)) f = (ErrVal (Right s))

instance Functor ErrVal where
    fmap f va = va >>= (return.f)

instance Applicative ErrVal where
    pure = return
    (<*>) = ap

evMaybe :: Maybe a -> String -> ErrVal a
evMaybe (Just v) _ = eVal v
evMaybe _ s = eErr s

evFilter :: [ErrVal a] -> [a]
evFilter = foldr f []
  where
    f (ErrVal (Left a)) as = a:as
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
errval fa fe (ErrVal (Left a)) = fa a
errval fa fe (ErrVal (Right e)) = fe e

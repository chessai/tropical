{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Tropical.Min 
  ( Tropical(Infinity,Min)
  , tropical
  , isTropical
  , isInfinity
  , fromMin
  , fromTropical
  , listToTropical
  , tropicalToList
  , catTropicals
  , mapTropical
  , (^) 
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), liftM2)
import Control.Monad.Zip (MonadZip(..))
import Data.Data (Data)
import Data.Functor.Classes
import Data.Type.Equality (type (==))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Semiring (Semiring(..))
import GHC.Base (build)
import GHC.Generics (Generic, Generic1)
import GHC.Read
import GHC.Real (Ratio(..))
import GHC.Show (appPrec)
import Prelude hiding ((^))
import Text.ParserCombinators.ReadPrec ((+++), prec, step)
import Text.Read.Lex (Lexeme(..))

data Tropical a = Min a | Infinity

tropical :: b -> (a -> b) -> Tropical a -> b
tropical n _ Infinity = n
tropical _ f (Min x) = f x

isTropical :: Tropical a -> Bool
isTropical (Min _) = True
isTropical _            = False

isInfinity :: Tropical a -> Bool
isInfinity Infinity = True
isInfinity _        = False

fromMin :: Monoid a => Tropical a -> a
fromMin (Min x) = x
fromMin _       = mempty

fromTropical :: a -> Tropical a -> a
fromTropical x Infinity = x
fromTropical _ (Min y)  = y

listToTropical :: [a] -> Tropical a
listToTropical []    = Infinity
listToTropical (x:_) = Min x

tropicalToList :: Tropical a -> [a]
tropicalToList Infinity = []
tropicalToList (Min x)  = [x]

catTropicals :: [Tropical a] -> [a]
catTropicals ls = [x | Min x <- ls]  

mapTropical :: (a -> Tropical b) -> [a] -> [b]
mapTropical _ [] = []
mapTropical f (x:xs) =
  let rs = mapTropical f xs in
  case f x of
    Infinity -> rs
    Min r    -> r : rs
{-# NOINLINE [1] mapTropical #-}

{-# RULES
"mapTropical"     [~1] forall f xs. mapTropical f xs
                    = build (\c n -> foldr (mapTropicalFB c f) n xs)
"mapTropicalList" [1]  forall f. foldr (mapTropicalFB (:) f) [] = mapTropical f
  #-}

{-# INLINE [0] mapTropicalFB #-} -- See Note [Inline FB functions] in GHC.List
mapTropicalFB :: (b -> r -> r) -> (a -> Tropical b) -> a -> r -> r
mapTropicalFB cons f x next = case f x of
  Infinity -> next
  Min r    -> cons r next

infixr 8 ^
{-# INLINABLE [1] (^) #-} -- See Note [Inline (^)] in GHC.Real
(^) :: Semiring a => Tropical a -> Tropical a -> Tropical a
Infinity ^ _ = Infinity
_ ^ Infinity = Infinity
(Min a) ^ (Min b) = Min (a `times` b)

{-----------------------------------------------------------------
 -- INSTANCES
 ----------------------------------------------------------------}

deriving instance Eq a   => Eq (Tropical a)
deriving instance Data a => Data (Tropical a)
deriving instance Ord a  => Ord (Tropical a)
deriving instance Show a => Show (Tropical a)
deriving instance Generic   (Tropical a)
deriving instance Generic1  Tropical

-- Most of these methods are nonsensical.
-- This is mostly just here for (+) and (*).
-- It is possible to remove the Ord constraint.
instance (Ord a, Semiring a) => Num (Tropical a) where
  (+)    = plus 
  (*)    = times 
  (-)    = max 
  negate = id
  abs    = max Infinity
  signum Infinity = zero
  signum _        = one
  fromInteger 0 = Infinity
  fromInteger _ = Min one 

instance (Ord a, Num a, Semiring a) => Fractional (Tropical a) where
  fromRational (x :% y) = (fromInteger x) / (fromInteger y) 
  
  _ / Infinity  = undefined
  Infinity / _  = Infinity
  Min a / Min b = Min (a - b)

instance Read a => Read (Tropical a) where
  readPrec =
    parens
    (do expectP (Ident "Infinity")
        pure Infinity
     +++
     prec appPrec (
        do expectP (Ident "Tropical")
           x <- step readPrec
           pure (Min x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
      
instance (Ord a, Semiring a) => Semiring (Tropical a) where
  zero = Infinity
  
  Infinity `plus` y = y
  x `plus` Infinity = x
  (Min a) `plus` (Min b) = Min (min a b)
  
  Infinity `times` _ = Infinity
  _ `times` Infinity = Infinity
  times (Min a) (Min b) = Min (times a b)
  
  one  = Min zero

instance Functor Tropical where
  fmap _ Infinity = Infinity
  fmap f (Min a) = Min (f a)

instance Applicative Tropical where
  pure = Min
  _ <*> Infinity = Infinity
  Infinity <*> _ = Infinity
  (Min f) <*> (Min a) = Min (f a)

instance Monad Tropical where
  return = pure
  (Min a) >>= f = f a
  Infinity     >>= _ = Infinity
  
  (>>) = (*>)
  
  fail _             = Infinity

instance Alternative Tropical where
  empty = Infinity
  Infinity <|> r = r
  l        <|> _ = l

instance MonadPlus Tropical where

instance Foldable Tropical where
  foldMap = tropical mempty

  foldr _ z Infinity = z
  foldr f z (Min x) = f x z

  foldl _ z Infinity = z
  foldl f z (Min x) = f z x

instance Traversable Tropical where
  traverse _ Infinity = pure Infinity
  traverse f (Min x) = Min <$> f x

instance Semigroup a => Semigroup (Tropical a) where
  Infinity <> y = y
  x <> Infinity = x
  (Min a) <> (Min b) = Min (a <> b)

instance Semigroup a => Monoid (Tropical a) where
  mempty = Infinity
  x `mappend` Infinity = x
  Infinity `mappend` y = y
  (Min a) `mappend` (Min b) = Min (a <> b)

instance MonadZip Tropical where
  mzipWith = liftM2

instance Show1 Tropical where
  liftShowsPrec _ _ _ Infinity = showString "Infinity"
  liftShowsPrec sp _ d (Min x) = showsUnaryWith sp "Tropical" d x

instance Read1 Tropical where
  liftReadPrec rp _ =
    parens (expectP (Ident "Tropical") *> pure Infinity)
    <|>
    readData (readUnaryWith rp "Tropical" Min)
  
  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

instance Eq1 Tropical where
  liftEq _ Infinity Infinity = True
  liftEq _ Infinity  _        = False
  liftEq _ _        Infinity = False
  liftEq eq (Min x) (Min y) = eq x y

instance Ord1 Tropical where
  liftCompare _ Infinity Infinity = EQ
  liftCompare _ Infinity _        = LT
  liftCompare _ _        Infinity = GT
  liftCompare comp (Min x) (Min y) = comp x y 

type family EqTropical a b where
  EqTropical 'Infinity 'Infinity = 'True
  EqTropical ('Min x)   ('Min y) = x == y
  EqTropical a         b         = 'False

type instance a == b = EqTropical a b

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Tropical.Max 
  ( Tropical(Infinity,Max)
  , tropical
  , isTropical
  , isInfinity
  , fromMax
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
import GHC.Show (appPrec)
import Prelude hiding ((^))
import Text.ParserCombinators.ReadPrec ((+++), prec, step)
import Text.Read.Lex (Lexeme(..))

data Tropical a = Max a | Infinity

tropical :: b -> (a -> b) -> Tropical a -> b
tropical n _ Infinity = n
tropical _ f (Max x) = f x

isTropical :: Tropical a -> Bool
isTropical (Max _) = True
isTropical _       = False

isInfinity :: Tropical a -> Bool
isInfinity Infinity = True
isInfinity _        = False

fromMax :: Monoid a => Tropical a -> a
fromMax (Max x) = x
fromMax _       = mempty

fromTropical :: a -> Tropical a -> a
fromTropical x Infinity = x
fromTropical _ (Max y)  = y

listToTropical :: [a] -> Tropical a
listToTropical []    = Infinity
listToTropical (x:_) = Max x

tropicalToList :: Tropical a -> [a]
tropicalToList Infinity = []
tropicalToList (Max x)  = [x]

catTropicals :: [Tropical a] -> [a]
catTropicals ls = [x | Max x <- ls]  

mapTropical :: (a -> Tropical b) -> [a] -> [b]
mapTropical _ [] = []
mapTropical f (x:xs) =
  let rs = mapTropical f xs in
  case f x of
    Infinity -> rs
    Max r    -> r : rs
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
  Max r    -> cons r next

infixr 8 ^
{-# INLINABLE [1] (^) #-} -- See Note [Inline (^)] in GHC.Real
(^) :: Semiring a => Tropical a -> Tropical a -> Tropical a
Infinity ^ _ = Infinity
_ ^ Infinity = Infinity
(Max a) ^ (Max b) = Max (a `times` b)

{-----------------------------------------------------------------
 -- INSTANCES
 ----------------------------------------------------------------}

deriving instance Eq a   => Eq (Tropical a)
deriving instance Data a => Data (Tropical a)
deriving instance Show a => Show (Tropical a)
deriving instance Generic   (Tropical a)
deriving instance Generic1  Tropical

instance (Ord a) => Ord (Tropical a) where
  compare _        Infinity = GT
  compare Infinity        _ = LT
  compare Infinity Infinity = EQ
  compare (Max x) (Max y)   = compare x y

-- Most of these methods are nonsensical.
-- This is mostly just here for (+) and (*).
-- It is possible to remove the Ord constraint.
instance (Ord a, Semiring a) => Num (Tropical a) where
  (+)    = plus 
  (*)    = times 
  (-)    = min 
  negate = id
  abs    = min Infinity
  signum Infinity = zero
  signum _        = one
  fromInteger 0 = Infinity
  fromInteger _ = Max one 

instance Read a => Read (Tropical a) where
  readPrec =
    parens
    (do expectP (Ident "Infinity")
        pure Infinity
     +++
     prec appPrec (
        do expectP (Ident "Tropical")
           x <- step readPrec
           pure (Max x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
      
instance (Ord a, Semiring a) => Semiring (Tropical a) where
  zero = Infinity
  
  Infinity `plus` y = y
  x `plus` Infinity = x
  (Max a) `plus` (Max b) = Max (max a b)
  
  Infinity `times` _ = Infinity
  _ `times` Infinity = Infinity
  times (Max a) (Max b) = Max (times a b)
  
  one  = Max zero

instance Functor Tropical where
  fmap _ Infinity = Infinity
  fmap f (Max a) = Max (f a)

instance Applicative Tropical where
  pure = Max
  _ <*> Infinity = Infinity
  Infinity <*> _ = Infinity
  (Max f) <*> (Max a) = Max (f a)

instance Monad Tropical where
  return = pure
  (Max a) >>= f = f a
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
  foldr f z (Max x) = f x z

  foldl _ z Infinity = z
  foldl f z (Max x) = f z x

instance Traversable Tropical where
  traverse _ Infinity = pure Infinity
  traverse f (Max x) = Max <$> f x

instance Semigroup a => Semigroup (Tropical a) where
  Infinity <> y = y
  x <> Infinity = x
  (Max a) <> (Max b) = Max (a <> b)

instance Semigroup a => Monoid (Tropical a) where
  mempty = Infinity
  x `mappend` Infinity = x
  Infinity `mappend` y = y
  (Max a) `mappend` (Max b) = Max (a <> b)

instance MonadZip Tropical where
  mzipWith = liftM2

instance Show1 Tropical where
  liftShowsPrec _ _ _ Infinity = showString "Infinity"
  liftShowsPrec sp _ d (Max x) = showsUnaryWith sp "Tropical" d x

instance Read1 Tropical where
  liftReadPrec rp _ =
    parens (expectP (Ident "Tropical") *> pure Infinity)
    <|>
    readData (readUnaryWith rp "Tropical" Max)
  
  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

instance Eq1 Tropical where
  liftEq _ Infinity Infinity = True
  liftEq _ Infinity  _        = False
  liftEq _ _        Infinity = False
  liftEq eq (Max x) (Max y) = eq x y

instance Ord1 Tropical where
  liftCompare _ Infinity Infinity = EQ
  liftCompare _ Infinity _        = LT
  liftCompare _ _        Infinity = GT
  liftCompare comp (Max x) (Max y) = comp x y 

type family EqTropical a b where
  EqTropical 'Infinity 'Infinity = 'True
  EqTropical ('Max x)   ('Max y) = x == y
  EqTropical a         b         = 'False

type instance a == b = EqTropical a b

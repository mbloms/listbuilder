{-# LANGUAGE ExplicitForAll,
             RankNTypes #-}

module Data.ListBuilder (module Data.ListBuilder, toList) where

import Prelude hiding (concat, head, last, tail, replicate, repeat)
import Data.Monoid hiding (First, getFirst, Last, getLast)
import Data.Semigroup
import Data.Foldable
import Control.Applicative
import Control.Monad
import GHC.Exts (build, augment)
import Data.Function (fix)

newtype ListBuilder a = Build (forall b. (a -> b -> b) -> b -> b)

fromList :: [a] -> ListBuilder a
fromList xs = Build $ \cons nil -> foldr cons nil xs

apply :: ListBuilder a -> [a] -> [a]
apply (Build g) = augment g

empty :: ListBuilder a
empty = mempty

singleton :: a -> ListBuilder a
singleton x = Build $ \cons xs -> cons x xs

cons :: a -> ListBuilder a -> ListBuilder a
cons x xs = pure x <> xs

snoc :: ListBuilder a -> a -> ListBuilder a
snoc xs x = xs <> pure x

append :: ListBuilder a -> ListBuilder a -> ListBuilder a
append = mappend

concat :: Foldable t => t (ListBuilder a) -> ListBuilder a
concat = fold

repeat :: a -> ListBuilder a
repeat x = Build $ \cons _ -> fix (cons x)

replicate l x = unfoldr (\i -> if i < l then Just (x, i+1) else Nothing) 0

head :: ListBuilder a -> Maybe a
head (Build g) = g (const . pure) Nothing

last :: ListBuilder a -> Maybe a
last (Build g) = g go Nothing
    where
        go x Nothing = Just x
        go _ mx = mx

-- stolen and adapted from unfoldr in Data.List
unfoldr :: (b -> Maybe (a, b)) -> b -> ListBuilder a
unfoldr f b0 = Build (\c n ->
    let go b = case f b of
                Just (a, new_b) -> a `c` go new_b
                Nothing         -> n
    in go b0)


instance Semigroup (ListBuilder a) where
    (<>) = mappend

instance Monoid (ListBuilder a) where
    mempty = Build $ \_ nil -> nil
    mappend (Build xfs) (Build yfs) = Build $ \cons -> xfs cons . yfs cons

instance Functor ListBuilder where
    -- Could have used liftM as fmap, but profiling indicates this is faster.
    -- Implementation uses the face that map can be implemented in terms of foldr:
    -- map f = foldr (\x xs -> f x : xs) []
    -- so fmap can be implemented as
    -- fmap f (Build g) = Build $ \cons nil -> foldr (\x xs -> cons (f x) xs) nil (build g)
    -- which is the same as:
    fmap f (Build g) = Build $ \cons nil -> g (\x xs -> cons (f x) xs) nil

instance Foldable ListBuilder where
    toList (Build f)    = build f
    foldr k z (Build g) = g k z
    fold (Build g)      = g mappend mempty
    foldMap f (Build g) = g (mappend . f) mempty

instance Applicative ListBuilder where
    pure = singleton
    liftA2 = liftM2

instance Monad ListBuilder where
    return = singleton
    (>>=) = flip foldMap

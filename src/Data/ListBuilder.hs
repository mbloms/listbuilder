{-# LANGUAGE ExplicitForAll,
             RankNTypes #-}

module Data.ListBuilder (module Data.ListBuilder, toList) where

import Data.Monoid
import Data.Foldable
import Control.Applicative
import Control.Monad
import GHC.Exts (build, augment)

newtype ListBuilder a = Build (forall b. (a -> b -> b) -> b -> b)

fromList :: [a] -> ListBuilder a
fromList xs = Build $ \cons nil -> foldr cons nil xs

singleton :: a -> ListBuilder a
singleton = return

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
    pure = return
    liftA2 = liftM2

instance Monad ListBuilder where
    return x = Build $ \cons xs -> cons x xs
    (>>=) = flip foldMap

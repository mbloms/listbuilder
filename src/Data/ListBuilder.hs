{-# LANGUAGE ExplicitForAll,
             RankNTypes #-}

module Data.ListBuilder where

import Data.Monoid
import Data.Foldable hiding (toList)
import Control.Applicative
import Control.Monad
import GHC.Exts (build, augment)

newtype ListBuilder a = Build (forall b. (a -> b -> b) -> b -> b)

fromList xs = Build $ \cons nil -> foldr cons nil xs

toList (Build f) = build f

instance Semigroup (ListBuilder a) where
    (<>) = mappend

instance Monoid (ListBuilder a) where
    mempty = Build $ \_ nil -> nil
    mappend (Build xfs) (Build yfs) = Build $ \cons -> xfs cons . yfs cons

instance Functor ListBuilder where
    fmap f (Build g) = Build $ \cons nil -> g (\x xs -> cons (f x) xs) nil

instance Foldable ListBuilder where
    foldr k z (Build g) = g k z
    fold (Build g) =      g mappend mempty
    foldMap f (Build g) = g (mappend . f) mempty

instance Applicative ListBuilder where
    pure = return
    liftA2 = liftM2

instance Monad ListBuilder where
    return x = Build $ \cons xs -> cons x xs
    (>>=) = flip foldMap

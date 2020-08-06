{-|
Module      : Lang.Crucible.Go.Rec
Description : TODO: short description
Maintainer  : abagnall@galois.com
Stability   : experimental

TODO: long description
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Language.Go.Rec where

import           Data.Functor.Product

import           Data.Parameterized.TraversableFC

import           Language.Go.Types

newtype Fix f i = In (f (Fix f) i)

out :: Fix f i -> f (Fix f) i
out (In f) = f

cata :: FunctorFC f =>
        (forall i. f a i -> a i) ->
        (forall i. Fix f i -> a i)
cata phi = phi . fmapFC (cata phi) . out

cataM :: (TraversableFC f, Monad m) =>
         (forall i. f a i -> m (a i)) ->
         (forall i. Fix f i -> m (a i))
cataM phi (In node) = traverseFC (cataM phi) node >>= phi

para :: FunctorFC f =>
        (forall i. f (Product (Fix f) a) i -> a i) ->
        (forall i. Fix f i -> a i)
para phi = phi . fmapFC (\x -> Pair x (para phi x)) . out

paraM :: (TraversableFC f, Monad m) =>
         (forall i. f (Product (Fix f) a) i -> m (a i)) ->
         (forall i. Fix f i -> m (a i))
paraM phi (In node) = traverseFC (\x -> Pair x <$> paraM phi x) node >>= phi

----------------------------------------------------------------------
-- Utility functions

pairM :: Applicative f => f a -> f b -> f (a, b)
pairM x y = (,) <$> x <*> y

proj1 :: forall f g (a :: NodeType). Product f g a -> f a
proj1 (Pair x _y) = x

proj2 :: forall f g (a :: NodeType). Product f g a -> g a
proj2 (Pair _x y) = y

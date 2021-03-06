{-|
Module      : Language.Go.Rec
Description : Golang recursion schemes
Maintainer  : abagnall@galois.com
Stability   : experimental

Recursion scheme combinators for Go abstract syntax.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Language.Go.Rec where

import           Data.Functor.Const
import           Data.Functor.Product

import           Data.Parameterized.TraversableFC

import           Language.Go.Types

newtype Fix f i = In (f (Fix f) i)

out :: Fix f i -> f (Fix f) i
out (In f) = f

cata :: FunctorFC f
     => (forall i. f a i -> a i)
     -> (forall i. Fix f i -> a i)
cata phi = phi . fmapFC (cata phi) . out

cata' :: FunctorFC f
      => (forall i. f (Const a) i -> a)
      -> (forall i. Fix f i -> a)
cata' phi = getConst . cata (Const . phi)

cataM :: (TraversableFC f, Monad m)
      => (forall i. f a i -> m (a i))
      -> (forall i. Fix f i -> m (a i))
cataM phi (In x) = traverseFC (cataM phi) x >>= phi

cataM' :: (TraversableFC f, Monad m)
       => (forall i. f (Const a) i -> m a)
       -> (forall i. Fix f i -> m a)
cataM' phi = (getConst <$>) . cataM ((Const <$>) . phi)

para :: FunctorFC f
     => (forall i. f (Product (Fix f) a) i -> a i)
     -> (forall i. Fix f i -> a i)
para phi = phi . fmapFC (\x -> Pair x (para phi x)) . out

paraM :: (TraversableFC f, Monad m)
      => (forall i. f (Product (Fix f) a) i -> m (a i))
      -> (forall i. Fix f i -> m (a i))
paraM phi (In x) = traverseFC (\y -> Pair y <$> paraM phi y) x >>= phi

----------------------------------------------------------------------
-- Utility functions

pairM :: Applicative f => f a -> f b -> f (a, b)
pairM x y = (,) <$> x <*> y

proj1 :: forall f g (a :: NodeType). Product f g a -> f a
proj1 (Pair x _y) = x

proj2 :: forall f g (a :: NodeType). Product f g a -> g a
proj2 (Pair _x y) = y

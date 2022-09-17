{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Core
  ( TestTree,
    testGroup,
    testProp,
    homomorphic1,
    homomorphic2,
  )
where

import Hedgehog (Gen, PropertyT, forAll, (===))

import Test.Compat (TestTree, testGroup, testProp)

--------------------------------------------------------------------------------

homomorphic1 ::
  (Eq b, Show a, Show b) =>
  Gen a ->
  (a -> a) ->
  (b -> b) ->
  (a -> b) ->
  PropertyT IO ()
homomorphic1 gen f g k = do
  x <- forAll gen
  k (f x) === g (k x)

homomorphic2 ::
  (Eq b, Show a, Show b) =>
  Gen a ->
  (a -> a -> a) ->
  (b -> b -> b) ->
  (a -> b) ->
  PropertyT IO ()
homomorphic2 gen f g k = do
  x <- forAll gen
  y <- forAll gen
  k (f x y) === g (k x) (k y)
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Compat 
  ( TestTree, 
    testGroup,
    testProp,
  )
where

import Hedgehog (Property)
import Test.Tasty (TestName, TestTree, testGroup)

#if MIN_VERSION_tasty_hedgehog(1,2,0)

import Data.String (fromString)
import Test.Tasty.Hedgehog (testPropertyNamed)

#else

import Test.Tasty.Hedgehog (testProperty)

#endif

--------------------------------------------------------------------------------

#if MIN_VERSION_tasty_hedgehog(1,2,0)

testProp :: TestName -> Property -> TestTree
testProp name = testPropertyNamed name (fromString name)

#else

testProp :: TestName -> Property -> TestTree
testProp = testProperty

#endif
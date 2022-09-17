{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitTable.Core
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.BitTable.Core
  ( -- * BitMap
    BitTable (BT),
  )
where

import GHC.Exts (SmallArray#)

-- include WORD_SIZE_IN_BITS
#include "MachDeps.h" 

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data BitTable a = BT (SmallArray# a)
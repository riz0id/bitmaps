{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.BitMap.Prim
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
module Data.BitMap.Prim.Core 
  ( BitMap# (BM#),
    bitmask#,
  )
where

import GHC.Exts (Int#, Word#)
import GHC.Exts qualified as GHC

-- include WORD_SIZE_IN_BITS
#include "MachDeps.h" 

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype BitMap# = BM# Word#

bitmask# :: Int# -> Word#
#if (WORD_SIZE_IN_BITS == 64)
bitmask# i# = GHC.uncheckedShiftRL# 0x8000000000000000## i#
#elif (WORD_SIZE_IN_BITS == 64)
bitmask# = GHC.uncheckedShiftRL# 0x80000000## 
#else
bitmask# i# =
  let i'# :: Int# 
      i'# = (WORD_SIZE_IN_BITS GHC.-# 1#) GHC.-# i#
   in GHC.uncheckedShiftL# 0x1## i# 
#endif
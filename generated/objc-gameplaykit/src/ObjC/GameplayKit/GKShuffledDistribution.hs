{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A shuffled distribution tries to make sure individual samples are not clustered whilst retaining a uniform distribution of values over time. This is often referred to as fair or less random, as the predicatability of the outcomes in a series is vastly increased, yet the distribution of values is uniform.
--
-- Do not use with distributions ranging more than 256 between lowest and highest as the shuffling seqeunce is stored internally in memory.
--
-- Generated bindings for @GKShuffledDistribution@.
module ObjC.GameplayKit.GKShuffledDistribution
  ( GKShuffledDistribution
  , IsGKShuffledDistribution(..)


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------


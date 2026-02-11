{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for acceleration structure descriptors. Do not use this class directly. Use one of the derived classes instead.
--
-- Generated bindings for @MTLAccelerationStructureDescriptor@.
module ObjC.Metal.MTLAccelerationStructureDescriptor
  ( MTLAccelerationStructureDescriptor
  , IsMTLAccelerationStructureDescriptor(..)
  , usage
  , setUsage
  , usageSelector
  , setUsageSelector

  -- * Enum types
  , MTLAccelerationStructureUsage(MTLAccelerationStructureUsage)
  , pattern MTLAccelerationStructureUsageNone
  , pattern MTLAccelerationStructureUsageRefit
  , pattern MTLAccelerationStructureUsagePreferFastBuild
  , pattern MTLAccelerationStructureUsageExtendedLimits
  , pattern MTLAccelerationStructureUsagePreferFastIntersection
  , pattern MTLAccelerationStructureUsageMinimizeMemory

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- usage@
usage :: IsMTLAccelerationStructureDescriptor mtlAccelerationStructureDescriptor => mtlAccelerationStructureDescriptor -> IO MTLAccelerationStructureUsage
usage mtlAccelerationStructureDescriptor  =
  fmap (coerce :: CULong -> MTLAccelerationStructureUsage) $ sendMsg mtlAccelerationStructureDescriptor (mkSelector "usage") retCULong []

-- | @- setUsage:@
setUsage :: IsMTLAccelerationStructureDescriptor mtlAccelerationStructureDescriptor => mtlAccelerationStructureDescriptor -> MTLAccelerationStructureUsage -> IO ()
setUsage mtlAccelerationStructureDescriptor  value =
  sendMsg mtlAccelerationStructureDescriptor (mkSelector "setUsage:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector
setUsageSelector = mkSelector "setUsage:"


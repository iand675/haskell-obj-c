{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setUsageSelector
  , usageSelector

  -- * Enum types
  , MTLAccelerationStructureUsage(MTLAccelerationStructureUsage)
  , pattern MTLAccelerationStructureUsageNone
  , pattern MTLAccelerationStructureUsageRefit
  , pattern MTLAccelerationStructureUsagePreferFastBuild
  , pattern MTLAccelerationStructureUsageExtendedLimits
  , pattern MTLAccelerationStructureUsagePreferFastIntersection
  , pattern MTLAccelerationStructureUsageMinimizeMemory

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- usage@
usage :: IsMTLAccelerationStructureDescriptor mtlAccelerationStructureDescriptor => mtlAccelerationStructureDescriptor -> IO MTLAccelerationStructureUsage
usage mtlAccelerationStructureDescriptor =
  sendMessage mtlAccelerationStructureDescriptor usageSelector

-- | @- setUsage:@
setUsage :: IsMTLAccelerationStructureDescriptor mtlAccelerationStructureDescriptor => mtlAccelerationStructureDescriptor -> MTLAccelerationStructureUsage -> IO ()
setUsage mtlAccelerationStructureDescriptor value =
  sendMessage mtlAccelerationStructureDescriptor setUsageSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @usage@
usageSelector :: Selector '[] MTLAccelerationStructureUsage
usageSelector = mkSelector "usage"

-- | @Selector@ for @setUsage:@
setUsageSelector :: Selector '[MTLAccelerationStructureUsage] ()
setUsageSelector = mkSelector "setUsage:"


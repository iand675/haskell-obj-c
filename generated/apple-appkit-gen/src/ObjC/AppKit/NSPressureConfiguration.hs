{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPressureConfiguration@.
module ObjC.AppKit.NSPressureConfiguration
  ( NSPressureConfiguration
  , IsNSPressureConfiguration(..)
  , initWithPressureBehavior
  , set
  , pressureBehavior
  , initWithPressureBehaviorSelector
  , pressureBehaviorSelector
  , setSelector

  -- * Enum types
  , NSPressureBehavior(NSPressureBehavior)
  , pattern NSPressureBehaviorUnknown
  , pattern NSPressureBehaviorPrimaryDefault
  , pattern NSPressureBehaviorPrimaryClick
  , pattern NSPressureBehaviorPrimaryGeneric
  , pattern NSPressureBehaviorPrimaryAccelerator
  , pattern NSPressureBehaviorPrimaryDeepClick
  , pattern NSPressureBehaviorPrimaryDeepDrag

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPressureBehavior:@
initWithPressureBehavior :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> NSPressureBehavior -> IO (Id NSPressureConfiguration)
initWithPressureBehavior nsPressureConfiguration pressureBehavior =
  sendOwnedMessage nsPressureConfiguration initWithPressureBehaviorSelector pressureBehavior

-- | @- set@
set :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> IO ()
set nsPressureConfiguration =
  sendMessage nsPressureConfiguration setSelector

-- | @- pressureBehavior@
pressureBehavior :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> IO NSPressureBehavior
pressureBehavior nsPressureConfiguration =
  sendMessage nsPressureConfiguration pressureBehaviorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPressureBehavior:@
initWithPressureBehaviorSelector :: Selector '[NSPressureBehavior] (Id NSPressureConfiguration)
initWithPressureBehaviorSelector = mkSelector "initWithPressureBehavior:"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @pressureBehavior@
pressureBehaviorSelector :: Selector '[] NSPressureBehavior
pressureBehaviorSelector = mkSelector "pressureBehavior"


{-# LANGUAGE PatternSynonyms #-}
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
  , setSelector
  , pressureBehaviorSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPressureBehavior:@
initWithPressureBehavior :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> NSPressureBehavior -> IO (Id NSPressureConfiguration)
initWithPressureBehavior nsPressureConfiguration  pressureBehavior =
  sendMsg nsPressureConfiguration (mkSelector "initWithPressureBehavior:") (retPtr retVoid) [argCLong (coerce pressureBehavior)] >>= ownedObject . castPtr

-- | @- set@
set :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> IO ()
set nsPressureConfiguration  =
  sendMsg nsPressureConfiguration (mkSelector "set") retVoid []

-- | @- pressureBehavior@
pressureBehavior :: IsNSPressureConfiguration nsPressureConfiguration => nsPressureConfiguration -> IO NSPressureBehavior
pressureBehavior nsPressureConfiguration  =
  fmap (coerce :: CLong -> NSPressureBehavior) $ sendMsg nsPressureConfiguration (mkSelector "pressureBehavior") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPressureBehavior:@
initWithPressureBehaviorSelector :: Selector
initWithPressureBehaviorSelector = mkSelector "initWithPressureBehavior:"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @pressureBehavior@
pressureBehaviorSelector :: Selector
pressureBehaviorSelector = mkSelector "pressureBehavior"


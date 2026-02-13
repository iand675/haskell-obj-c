{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicClusterStartUpEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicClusterStartUpEvent
  ( MTRBridgedDeviceBasicClusterStartUpEvent
  , IsMTRBridgedDeviceBasicClusterStartUpEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , setSoftwareVersionSelector
  , softwareVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- softwareVersion@
softwareVersion :: IsMTRBridgedDeviceBasicClusterStartUpEvent mtrBridgedDeviceBasicClusterStartUpEvent => mtrBridgedDeviceBasicClusterStartUpEvent -> IO (Id NSNumber)
softwareVersion mtrBridgedDeviceBasicClusterStartUpEvent =
  sendMessage mtrBridgedDeviceBasicClusterStartUpEvent softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTRBridgedDeviceBasicClusterStartUpEvent mtrBridgedDeviceBasicClusterStartUpEvent, IsNSNumber value) => mtrBridgedDeviceBasicClusterStartUpEvent -> value -> IO ()
setSoftwareVersion mtrBridgedDeviceBasicClusterStartUpEvent value =
  sendMessage mtrBridgedDeviceBasicClusterStartUpEvent setSoftwareVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"


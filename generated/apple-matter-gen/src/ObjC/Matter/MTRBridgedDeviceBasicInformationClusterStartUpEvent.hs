{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterStartUpEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterStartUpEvent
  ( MTRBridgedDeviceBasicInformationClusterStartUpEvent
  , IsMTRBridgedDeviceBasicInformationClusterStartUpEvent(..)
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
softwareVersion :: IsMTRBridgedDeviceBasicInformationClusterStartUpEvent mtrBridgedDeviceBasicInformationClusterStartUpEvent => mtrBridgedDeviceBasicInformationClusterStartUpEvent -> IO (Id NSNumber)
softwareVersion mtrBridgedDeviceBasicInformationClusterStartUpEvent =
  sendMessage mtrBridgedDeviceBasicInformationClusterStartUpEvent softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTRBridgedDeviceBasicInformationClusterStartUpEvent mtrBridgedDeviceBasicInformationClusterStartUpEvent, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterStartUpEvent -> value -> IO ()
setSoftwareVersion mtrBridgedDeviceBasicInformationClusterStartUpEvent value =
  sendMessage mtrBridgedDeviceBasicInformationClusterStartUpEvent setSoftwareVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"


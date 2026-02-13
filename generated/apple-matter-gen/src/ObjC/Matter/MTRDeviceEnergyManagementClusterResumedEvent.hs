{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterResumedEvent@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterResumedEvent
  ( MTRDeviceEnergyManagementClusterResumedEvent
  , IsMTRDeviceEnergyManagementClusterResumedEvent(..)
  , cause
  , setCause
  , causeSelector
  , setCauseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterResumedEvent mtrDeviceEnergyManagementClusterResumedEvent => mtrDeviceEnergyManagementClusterResumedEvent -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterResumedEvent =
  sendMessage mtrDeviceEnergyManagementClusterResumedEvent causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterResumedEvent mtrDeviceEnergyManagementClusterResumedEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterResumedEvent -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterResumedEvent value =
  sendMessage mtrDeviceEnergyManagementClusterResumedEvent setCauseSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cause@
causeSelector :: Selector '[] (Id NSNumber)
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector '[Id NSNumber] ()
setCauseSelector = mkSelector "setCause:"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterModeTagStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterModeTagStruct
  ( MTRDeviceEnergyManagementModeClusterModeTagStruct
  , IsMTRDeviceEnergyManagementModeClusterModeTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mfgCode@
mfgCode :: IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct => mtrDeviceEnergyManagementModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrDeviceEnergyManagementModeClusterModeTagStruct =
  sendMessage mtrDeviceEnergyManagementModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrDeviceEnergyManagementModeClusterModeTagStruct value =
  sendMessage mtrDeviceEnergyManagementModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct => mtrDeviceEnergyManagementModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrDeviceEnergyManagementModeClusterModeTagStruct =
  sendMessage mtrDeviceEnergyManagementModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeTagStruct -> value -> IO ()
setValue mtrDeviceEnergyManagementModeClusterModeTagStruct value =
  sendMessage mtrDeviceEnergyManagementModeClusterModeTagStruct setValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector '[] (Id NSNumber)
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector '[Id NSNumber] ()
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"


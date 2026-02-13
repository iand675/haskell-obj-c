{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterModeOptionStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterModeOptionStruct
  ( MTRDeviceEnergyManagementModeClusterModeOptionStruct
  , IsMTRDeviceEnergyManagementModeClusterModeOptionStruct(..)
  , label
  , setLabel
  , mode
  , setMode
  , modeTags
  , setModeTags
  , labelSelector
  , modeSelector
  , modeTagsSelector
  , setLabelSelector
  , setModeSelector
  , setModeTagsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- label@
label :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSString)
label mtrDeviceEnergyManagementModeClusterModeOptionStruct =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSString value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrDeviceEnergyManagementModeClusterModeOptionStruct value =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrDeviceEnergyManagementModeClusterModeOptionStruct =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setMode mtrDeviceEnergyManagementModeClusterModeOptionStruct value =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrDeviceEnergyManagementModeClusterModeOptionStruct =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSArray value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrDeviceEnergyManagementModeClusterModeOptionStruct value =
  sendMessage mtrDeviceEnergyManagementModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] (Id NSNumber)
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[Id NSNumber] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @modeTags@
modeTagsSelector :: Selector '[] (Id NSArray)
modeTagsSelector = mkSelector "modeTags"

-- | @Selector@ for @setModeTags:@
setModeTagsSelector :: Selector '[Id NSArray] ()
setModeTagsSelector = mkSelector "setModeTags:"


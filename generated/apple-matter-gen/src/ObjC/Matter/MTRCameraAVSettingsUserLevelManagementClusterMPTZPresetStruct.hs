{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct(..)
  , presetID
  , setPresetID
  , name
  , setName
  , settings
  , setSettings
  , nameSelector
  , presetIDSelector
  , setNameSelector
  , setPresetIDSelector
  , setSettingsSelector
  , settingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presetID@
presetID :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id NSNumber)
presetID mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct presetIDSelector

-- | @- setPresetID:@
setPresetID :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setPresetID mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct setPresetIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id NSString)
name mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct nameSelector

-- | @- setName:@
setName :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsNSString value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setName mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct setNameSelector (toNSString value)

-- | @- settings@
settings :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct)
settings mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct settingsSelector

-- | @- setSettings:@
setSettings :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setSettings mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct setSettingsSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetID@
presetIDSelector :: Selector '[] (Id NSNumber)
presetIDSelector = mkSelector "presetID"

-- | @Selector@ for @setPresetID:@
setPresetIDSelector :: Selector '[Id NSNumber] ()
setPresetIDSelector = mkSelector "setPresetID:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct)
settingsSelector = mkSelector "settings"

-- | @Selector@ for @setSettings:@
setSettingsSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct] ()
setSettingsSelector = mkSelector "setSettings:"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterThermostatSuggestionStruct@.
module ObjC.Matter.MTRThermostatClusterThermostatSuggestionStruct
  ( MTRThermostatClusterThermostatSuggestionStruct
  , IsMTRThermostatClusterThermostatSuggestionStruct(..)
  , uniqueID
  , setUniqueID
  , presetHandle
  , setPresetHandle
  , effectiveTime
  , setEffectiveTime
  , expirationTime
  , setExpirationTime
  , effectiveTimeSelector
  , expirationTimeSelector
  , presetHandleSelector
  , setEffectiveTimeSelector
  , setExpirationTimeSelector
  , setPresetHandleSelector
  , setUniqueIDSelector
  , uniqueIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- uniqueID@
uniqueID :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
uniqueID mtrThermostatClusterThermostatSuggestionStruct =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct uniqueIDSelector

-- | @- setUniqueID:@
setUniqueID :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setUniqueID mtrThermostatClusterThermostatSuggestionStruct value =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct setUniqueIDSelector (toNSNumber value)

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterThermostatSuggestionStruct =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct presetHandleSelector

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSData value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterThermostatSuggestionStruct value =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct setPresetHandleSelector (toNSData value)

-- | @- effectiveTime@
effectiveTime :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
effectiveTime mtrThermostatClusterThermostatSuggestionStruct =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct effectiveTimeSelector

-- | @- setEffectiveTime:@
setEffectiveTime :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setEffectiveTime mtrThermostatClusterThermostatSuggestionStruct value =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct setEffectiveTimeSelector (toNSNumber value)

-- | @- expirationTime@
expirationTime :: IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct => mtrThermostatClusterThermostatSuggestionStruct -> IO (Id NSNumber)
expirationTime mtrThermostatClusterThermostatSuggestionStruct =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct expirationTimeSelector

-- | @- setExpirationTime:@
setExpirationTime :: (IsMTRThermostatClusterThermostatSuggestionStruct mtrThermostatClusterThermostatSuggestionStruct, IsNSNumber value) => mtrThermostatClusterThermostatSuggestionStruct -> value -> IO ()
setExpirationTime mtrThermostatClusterThermostatSuggestionStruct value =
  sendMessage mtrThermostatClusterThermostatSuggestionStruct setExpirationTimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] (Id NSNumber)
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector '[Id NSNumber] ()
setUniqueIDSelector = mkSelector "setUniqueID:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector '[] (Id NSData)
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector '[Id NSData] ()
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @effectiveTime@
effectiveTimeSelector :: Selector '[] (Id NSNumber)
effectiveTimeSelector = mkSelector "effectiveTime"

-- | @Selector@ for @setEffectiveTime:@
setEffectiveTimeSelector :: Selector '[Id NSNumber] ()
setEffectiveTimeSelector = mkSelector "setEffectiveTime:"

-- | @Selector@ for @expirationTime@
expirationTimeSelector :: Selector '[] (Id NSNumber)
expirationTimeSelector = mkSelector "expirationTime"

-- | @Selector@ for @setExpirationTime:@
setExpirationTimeSelector :: Selector '[Id NSNumber] ()
setExpirationTimeSelector = mkSelector "setExpirationTime:"


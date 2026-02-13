{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct
  ( MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct
  , IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct(..)
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
label :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSString)
label mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSString value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setMode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSArray value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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


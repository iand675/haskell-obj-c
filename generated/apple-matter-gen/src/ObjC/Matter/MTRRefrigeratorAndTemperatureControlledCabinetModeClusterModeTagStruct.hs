{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct@.
module ObjC.Matter.MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct
  ( MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct
  , IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> value -> IO ()
setValue mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct value =
  sendMessage mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct setValueSelector (toNSNumber value)

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


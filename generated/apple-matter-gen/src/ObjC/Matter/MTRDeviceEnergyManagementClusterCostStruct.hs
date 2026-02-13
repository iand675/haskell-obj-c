{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterCostStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterCostStruct
  ( MTRDeviceEnergyManagementClusterCostStruct
  , IsMTRDeviceEnergyManagementClusterCostStruct(..)
  , costType
  , setCostType
  , value
  , setValue
  , decimalPoints
  , setDecimalPoints
  , currency
  , setCurrency
  , costTypeSelector
  , currencySelector
  , decimalPointsSelector
  , setCostTypeSelector
  , setCurrencySelector
  , setDecimalPointsSelector
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

-- | @- costType@
costType :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
costType mtrDeviceEnergyManagementClusterCostStruct =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct costTypeSelector

-- | @- setCostType:@
setCostType :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setCostType mtrDeviceEnergyManagementClusterCostStruct value =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct setCostTypeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
value mtrDeviceEnergyManagementClusterCostStruct =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setValue mtrDeviceEnergyManagementClusterCostStruct value =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct setValueSelector (toNSNumber value)

-- | @- decimalPoints@
decimalPoints :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
decimalPoints mtrDeviceEnergyManagementClusterCostStruct =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct decimalPointsSelector

-- | @- setDecimalPoints:@
setDecimalPoints :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setDecimalPoints mtrDeviceEnergyManagementClusterCostStruct value =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct setDecimalPointsSelector (toNSNumber value)

-- | @- currency@
currency :: IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct => mtrDeviceEnergyManagementClusterCostStruct -> IO (Id NSNumber)
currency mtrDeviceEnergyManagementClusterCostStruct =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct currencySelector

-- | @- setCurrency:@
setCurrency :: (IsMTRDeviceEnergyManagementClusterCostStruct mtrDeviceEnergyManagementClusterCostStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterCostStruct -> value -> IO ()
setCurrency mtrDeviceEnergyManagementClusterCostStruct value =
  sendMessage mtrDeviceEnergyManagementClusterCostStruct setCurrencySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @costType@
costTypeSelector :: Selector '[] (Id NSNumber)
costTypeSelector = mkSelector "costType"

-- | @Selector@ for @setCostType:@
setCostTypeSelector :: Selector '[Id NSNumber] ()
setCostTypeSelector = mkSelector "setCostType:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @decimalPoints@
decimalPointsSelector :: Selector '[] (Id NSNumber)
decimalPointsSelector = mkSelector "decimalPoints"

-- | @Selector@ for @setDecimalPoints:@
setDecimalPointsSelector :: Selector '[Id NSNumber] ()
setDecimalPointsSelector = mkSelector "setDecimalPoints:"

-- | @Selector@ for @currency@
currencySelector :: Selector '[] (Id NSNumber)
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector '[Id NSNumber] ()
setCurrencySelector = mkSelector "setCurrency:"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct@.
module ObjC.Matter.MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct
  ( MTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct
  , IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct(..)
  , number
  , setNumber
  , requiredState
  , setRequiredState
  , numberSelector
  , requiredStateSelector
  , setNumberSelector
  , setRequiredStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- number@
number :: IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> IO (Id NSNumber)
number mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct =
  sendMessage mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct numberSelector

-- | @- setNumber:@
setNumber :: (IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct, IsNSNumber value) => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> value -> IO ()
setNumber mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct value =
  sendMessage mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct setNumberSelector (toNSNumber value)

-- | @- requiredState@
requiredState :: IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> IO (Id NSNumber)
requiredState mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct =
  sendMessage mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct requiredStateSelector

-- | @- setRequiredState:@
setRequiredState :: (IsMTRCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct, IsNSNumber value) => mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct -> value -> IO ()
setRequiredState mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct value =
  sendMessage mtrCommodityTariffClusterAuxiliaryLoadSwitchSettingsStruct setRequiredStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @number@
numberSelector :: Selector '[] (Id NSNumber)
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector '[Id NSNumber] ()
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @requiredState@
requiredStateSelector :: Selector '[] (Id NSNumber)
requiredStateSelector = mkSelector "requiredState"

-- | @Selector@ for @setRequiredState:@
setRequiredStateSelector :: Selector '[Id NSNumber] ()
setRequiredStateSelector = mkSelector "setRequiredState:"


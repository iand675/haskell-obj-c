{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffInformationStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffInformationStruct
  ( MTRCommodityTariffClusterTariffInformationStruct
  , IsMTRCommodityTariffClusterTariffInformationStruct(..)
  , tariffLabel
  , setTariffLabel
  , providerName
  , setProviderName
  , currency
  , setCurrency
  , blockMode
  , setBlockMode
  , blockModeSelector
  , currencySelector
  , providerNameSelector
  , setBlockModeSelector
  , setCurrencySelector
  , setProviderNameSelector
  , setTariffLabelSelector
  , tariffLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- tariffLabel@
tariffLabel :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSString)
tariffLabel mtrCommodityTariffClusterTariffInformationStruct =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct tariffLabelSelector

-- | @- setTariffLabel:@
setTariffLabel :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSString value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setTariffLabel mtrCommodityTariffClusterTariffInformationStruct value =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct setTariffLabelSelector (toNSString value)

-- | @- providerName@
providerName :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSString)
providerName mtrCommodityTariffClusterTariffInformationStruct =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct providerNameSelector

-- | @- setProviderName:@
setProviderName :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSString value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setProviderName mtrCommodityTariffClusterTariffInformationStruct value =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct setProviderNameSelector (toNSString value)

-- | @- currency@
currency :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id MTRDataTypeCurrencyStruct)
currency mtrCommodityTariffClusterTariffInformationStruct =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct currencySelector

-- | @- setCurrency:@
setCurrency :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsMTRDataTypeCurrencyStruct value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setCurrency mtrCommodityTariffClusterTariffInformationStruct value =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct setCurrencySelector (toMTRDataTypeCurrencyStruct value)

-- | @- blockMode@
blockMode :: IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct => mtrCommodityTariffClusterTariffInformationStruct -> IO (Id NSNumber)
blockMode mtrCommodityTariffClusterTariffInformationStruct =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct blockModeSelector

-- | @- setBlockMode:@
setBlockMode :: (IsMTRCommodityTariffClusterTariffInformationStruct mtrCommodityTariffClusterTariffInformationStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffInformationStruct -> value -> IO ()
setBlockMode mtrCommodityTariffClusterTariffInformationStruct value =
  sendMessage mtrCommodityTariffClusterTariffInformationStruct setBlockModeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffLabel@
tariffLabelSelector :: Selector '[] (Id NSString)
tariffLabelSelector = mkSelector "tariffLabel"

-- | @Selector@ for @setTariffLabel:@
setTariffLabelSelector :: Selector '[Id NSString] ()
setTariffLabelSelector = mkSelector "setTariffLabel:"

-- | @Selector@ for @providerName@
providerNameSelector :: Selector '[] (Id NSString)
providerNameSelector = mkSelector "providerName"

-- | @Selector@ for @setProviderName:@
setProviderNameSelector :: Selector '[Id NSString] ()
setProviderNameSelector = mkSelector "setProviderName:"

-- | @Selector@ for @currency@
currencySelector :: Selector '[] (Id MTRDataTypeCurrencyStruct)
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector '[Id MTRDataTypeCurrencyStruct] ()
setCurrencySelector = mkSelector "setCurrency:"

-- | @Selector@ for @blockMode@
blockModeSelector :: Selector '[] (Id NSNumber)
blockModeSelector = mkSelector "blockMode"

-- | @Selector@ for @setBlockMode:@
setBlockModeSelector :: Selector '[Id NSNumber] ()
setBlockModeSelector = mkSelector "setBlockMode:"


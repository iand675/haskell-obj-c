{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffPriceStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffPriceStruct
  ( MTRCommodityTariffClusterTariffPriceStruct
  , IsMTRCommodityTariffClusterTariffPriceStruct(..)
  , priceType
  , setPriceType
  , price
  , setPrice
  , priceLevel
  , setPriceLevel
  , priceLevelSelector
  , priceSelector
  , priceTypeSelector
  , setPriceLevelSelector
  , setPriceSelector
  , setPriceTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- priceType@
priceType :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
priceType mtrCommodityTariffClusterTariffPriceStruct =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct priceTypeSelector

-- | @- setPriceType:@
setPriceType :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPriceType mtrCommodityTariffClusterTariffPriceStruct value =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct setPriceTypeSelector (toNSNumber value)

-- | @- price@
price :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
price mtrCommodityTariffClusterTariffPriceStruct =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct priceSelector

-- | @- setPrice:@
setPrice :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPrice mtrCommodityTariffClusterTariffPriceStruct value =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct setPriceSelector (toNSNumber value)

-- | @- priceLevel@
priceLevel :: IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct => mtrCommodityTariffClusterTariffPriceStruct -> IO (Id NSNumber)
priceLevel mtrCommodityTariffClusterTariffPriceStruct =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct priceLevelSelector

-- | @- setPriceLevel:@
setPriceLevel :: (IsMTRCommodityTariffClusterTariffPriceStruct mtrCommodityTariffClusterTariffPriceStruct, IsNSNumber value) => mtrCommodityTariffClusterTariffPriceStruct -> value -> IO ()
setPriceLevel mtrCommodityTariffClusterTariffPriceStruct value =
  sendMessage mtrCommodityTariffClusterTariffPriceStruct setPriceLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @priceType@
priceTypeSelector :: Selector '[] (Id NSNumber)
priceTypeSelector = mkSelector "priceType"

-- | @Selector@ for @setPriceType:@
setPriceTypeSelector :: Selector '[Id NSNumber] ()
setPriceTypeSelector = mkSelector "setPriceType:"

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id NSNumber)
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector '[Id NSNumber] ()
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @priceLevel@
priceLevelSelector :: Selector '[] (Id NSNumber)
priceLevelSelector = mkSelector "priceLevel"

-- | @Selector@ for @setPriceLevel:@
setPriceLevelSelector :: Selector '[Id NSNumber] ()
setPriceLevelSelector = mkSelector "setPriceLevel:"


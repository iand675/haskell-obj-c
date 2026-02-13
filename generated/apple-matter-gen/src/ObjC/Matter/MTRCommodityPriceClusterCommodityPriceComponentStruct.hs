{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterCommodityPriceComponentStruct@.
module ObjC.Matter.MTRCommodityPriceClusterCommodityPriceComponentStruct
  ( MTRCommodityPriceClusterCommodityPriceComponentStruct
  , IsMTRCommodityPriceClusterCommodityPriceComponentStruct(..)
  , price
  , setPrice
  , source
  , setSource
  , descriptionString
  , setDescriptionString
  , tariffComponentID
  , setTariffComponentID
  , descriptionStringSelector
  , priceSelector
  , setDescriptionStringSelector
  , setPriceSelector
  , setSourceSelector
  , setTariffComponentIDSelector
  , sourceSelector
  , tariffComponentIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- price@
price :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
price mtrCommodityPriceClusterCommodityPriceComponentStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct priceSelector

-- | @- setPrice:@
setPrice :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setPrice mtrCommodityPriceClusterCommodityPriceComponentStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct setPriceSelector (toNSNumber value)

-- | @- source@
source :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
source mtrCommodityPriceClusterCommodityPriceComponentStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct sourceSelector

-- | @- setSource:@
setSource :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setSource mtrCommodityPriceClusterCommodityPriceComponentStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct setSourceSelector (toNSNumber value)

-- | @- descriptionString@
descriptionString :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSString)
descriptionString mtrCommodityPriceClusterCommodityPriceComponentStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct descriptionStringSelector

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSString value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setDescriptionString mtrCommodityPriceClusterCommodityPriceComponentStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct setDescriptionStringSelector (toNSString value)

-- | @- tariffComponentID@
tariffComponentID :: IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct => mtrCommodityPriceClusterCommodityPriceComponentStruct -> IO (Id NSNumber)
tariffComponentID mtrCommodityPriceClusterCommodityPriceComponentStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct tariffComponentIDSelector

-- | @- setTariffComponentID:@
setTariffComponentID :: (IsMTRCommodityPriceClusterCommodityPriceComponentStruct mtrCommodityPriceClusterCommodityPriceComponentStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceComponentStruct -> value -> IO ()
setTariffComponentID mtrCommodityPriceClusterCommodityPriceComponentStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceComponentStruct setTariffComponentIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id NSNumber)
priceSelector = mkSelector "price"

-- | @Selector@ for @setPrice:@
setPriceSelector :: Selector '[Id NSNumber] ()
setPriceSelector = mkSelector "setPrice:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSNumber)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id NSNumber] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector '[] (Id NSString)
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector '[Id NSString] ()
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @tariffComponentID@
tariffComponentIDSelector :: Selector '[] (Id NSNumber)
tariffComponentIDSelector = mkSelector "tariffComponentID"

-- | @Selector@ for @setTariffComponentID:@
setTariffComponentIDSelector :: Selector '[Id NSNumber] ()
setTariffComponentIDSelector = mkSelector "setTariffComponentID:"


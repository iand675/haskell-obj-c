{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterCommodityPriceStruct@.
module ObjC.Matter.MTRCommodityPriceClusterCommodityPriceStruct
  ( MTRCommodityPriceClusterCommodityPriceStruct
  , IsMTRCommodityPriceClusterCommodityPriceStruct(..)
  , periodStart
  , setPeriodStart
  , periodEnd
  , setPeriodEnd
  , price
  , setPrice
  , priceLevel
  , setPriceLevel
  , descriptionString
  , setDescriptionString
  , components
  , setComponents
  , componentsSelector
  , descriptionStringSelector
  , periodEndSelector
  , periodStartSelector
  , priceLevelSelector
  , priceSelector
  , setComponentsSelector
  , setDescriptionStringSelector
  , setPeriodEndSelector
  , setPeriodStartSelector
  , setPriceLevelSelector
  , setPriceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- periodStart@
periodStart :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
periodStart mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct periodStartSelector

-- | @- setPeriodStart:@
setPeriodStart :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPeriodStart mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setPeriodStartSelector (toNSNumber value)

-- | @- periodEnd@
periodEnd :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
periodEnd mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct periodEndSelector

-- | @- setPeriodEnd:@
setPeriodEnd :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPeriodEnd mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setPeriodEndSelector (toNSNumber value)

-- | @- price@
price :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
price mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct priceSelector

-- | @- setPrice:@
setPrice :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPrice mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setPriceSelector (toNSNumber value)

-- | @- priceLevel@
priceLevel :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSNumber)
priceLevel mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct priceLevelSelector

-- | @- setPriceLevel:@
setPriceLevel :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSNumber value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setPriceLevel mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setPriceLevelSelector (toNSNumber value)

-- | @- descriptionString@
descriptionString :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSString)
descriptionString mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct descriptionStringSelector

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSString value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setDescriptionString mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setDescriptionStringSelector (toNSString value)

-- | @- components@
components :: IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct => mtrCommodityPriceClusterCommodityPriceStruct -> IO (Id NSArray)
components mtrCommodityPriceClusterCommodityPriceStruct =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct componentsSelector

-- | @- setComponents:@
setComponents :: (IsMTRCommodityPriceClusterCommodityPriceStruct mtrCommodityPriceClusterCommodityPriceStruct, IsNSArray value) => mtrCommodityPriceClusterCommodityPriceStruct -> value -> IO ()
setComponents mtrCommodityPriceClusterCommodityPriceStruct value =
  sendMessage mtrCommodityPriceClusterCommodityPriceStruct setComponentsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @periodStart@
periodStartSelector :: Selector '[] (Id NSNumber)
periodStartSelector = mkSelector "periodStart"

-- | @Selector@ for @setPeriodStart:@
setPeriodStartSelector :: Selector '[Id NSNumber] ()
setPeriodStartSelector = mkSelector "setPeriodStart:"

-- | @Selector@ for @periodEnd@
periodEndSelector :: Selector '[] (Id NSNumber)
periodEndSelector = mkSelector "periodEnd"

-- | @Selector@ for @setPeriodEnd:@
setPeriodEndSelector :: Selector '[Id NSNumber] ()
setPeriodEndSelector = mkSelector "setPeriodEnd:"

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

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector '[] (Id NSString)
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector '[Id NSString] ()
setDescriptionStringSelector = mkSelector "setDescriptionString:"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSArray)
componentsSelector = mkSelector "components"

-- | @Selector@ for @setComponents:@
setComponentsSelector :: Selector '[Id NSArray] ()
setComponentsSelector = mkSelector "setComponents:"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityMeteringClusterMeteredQuantityStruct@.
module ObjC.Matter.MTRCommodityMeteringClusterMeteredQuantityStruct
  ( MTRCommodityMeteringClusterMeteredQuantityStruct
  , IsMTRCommodityMeteringClusterMeteredQuantityStruct(..)
  , tariffComponentIDs
  , setTariffComponentIDs
  , quantity
  , setQuantity
  , quantitySelector
  , setQuantitySelector
  , setTariffComponentIDsSelector
  , tariffComponentIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- tariffComponentIDs@
tariffComponentIDs :: IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct => mtrCommodityMeteringClusterMeteredQuantityStruct -> IO (Id NSArray)
tariffComponentIDs mtrCommodityMeteringClusterMeteredQuantityStruct =
  sendMessage mtrCommodityMeteringClusterMeteredQuantityStruct tariffComponentIDsSelector

-- | @- setTariffComponentIDs:@
setTariffComponentIDs :: (IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct, IsNSArray value) => mtrCommodityMeteringClusterMeteredQuantityStruct -> value -> IO ()
setTariffComponentIDs mtrCommodityMeteringClusterMeteredQuantityStruct value =
  sendMessage mtrCommodityMeteringClusterMeteredQuantityStruct setTariffComponentIDsSelector (toNSArray value)

-- | @- quantity@
quantity :: IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct => mtrCommodityMeteringClusterMeteredQuantityStruct -> IO (Id NSNumber)
quantity mtrCommodityMeteringClusterMeteredQuantityStruct =
  sendMessage mtrCommodityMeteringClusterMeteredQuantityStruct quantitySelector

-- | @- setQuantity:@
setQuantity :: (IsMTRCommodityMeteringClusterMeteredQuantityStruct mtrCommodityMeteringClusterMeteredQuantityStruct, IsNSNumber value) => mtrCommodityMeteringClusterMeteredQuantityStruct -> value -> IO ()
setQuantity mtrCommodityMeteringClusterMeteredQuantityStruct value =
  sendMessage mtrCommodityMeteringClusterMeteredQuantityStruct setQuantitySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tariffComponentIDs@
tariffComponentIDsSelector :: Selector '[] (Id NSArray)
tariffComponentIDsSelector = mkSelector "tariffComponentIDs"

-- | @Selector@ for @setTariffComponentIDs:@
setTariffComponentIDsSelector :: Selector '[Id NSArray] ()
setTariffComponentIDsSelector = mkSelector "setTariffComponentIDs:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] (Id NSNumber)
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector '[Id NSNumber] ()
setQuantitySelector = mkSelector "setQuantity:"


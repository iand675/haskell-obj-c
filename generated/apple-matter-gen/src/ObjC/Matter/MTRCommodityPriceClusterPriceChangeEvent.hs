{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterPriceChangeEvent@.
module ObjC.Matter.MTRCommodityPriceClusterPriceChangeEvent
  ( MTRCommodityPriceClusterPriceChangeEvent
  , IsMTRCommodityPriceClusterPriceChangeEvent(..)
  , currentPrice
  , setCurrentPrice
  , currentPriceSelector
  , setCurrentPriceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- currentPrice@
currentPrice :: IsMTRCommodityPriceClusterPriceChangeEvent mtrCommodityPriceClusterPriceChangeEvent => mtrCommodityPriceClusterPriceChangeEvent -> IO (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPrice mtrCommodityPriceClusterPriceChangeEvent =
  sendMessage mtrCommodityPriceClusterPriceChangeEvent currentPriceSelector

-- | @- setCurrentPrice:@
setCurrentPrice :: (IsMTRCommodityPriceClusterPriceChangeEvent mtrCommodityPriceClusterPriceChangeEvent, IsMTRCommodityPriceClusterCommodityPriceStruct value) => mtrCommodityPriceClusterPriceChangeEvent -> value -> IO ()
setCurrentPrice mtrCommodityPriceClusterPriceChangeEvent value =
  sendMessage mtrCommodityPriceClusterPriceChangeEvent setCurrentPriceSelector (toMTRCommodityPriceClusterCommodityPriceStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentPrice@
currentPriceSelector :: Selector '[] (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPriceSelector = mkSelector "currentPrice"

-- | @Selector@ for @setCurrentPrice:@
setCurrentPriceSelector :: Selector '[Id MTRCommodityPriceClusterCommodityPriceStruct] ()
setCurrentPriceSelector = mkSelector "setCurrentPrice:"


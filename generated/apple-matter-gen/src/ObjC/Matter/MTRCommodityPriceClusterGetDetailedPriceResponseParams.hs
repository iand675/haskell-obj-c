{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterGetDetailedPriceResponseParams@.
module ObjC.Matter.MTRCommodityPriceClusterGetDetailedPriceResponseParams
  ( MTRCommodityPriceClusterGetDetailedPriceResponseParams
  , IsMTRCommodityPriceClusterGetDetailedPriceResponseParams(..)
  , initWithResponseValue_error
  , currentPrice
  , setCurrentPrice
  , currentPriceSelector
  , initWithResponseValue_errorSelector
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

-- | Initialize an MTRCommodityPriceClusterGetDetailedPriceResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityPriceClusterGetDetailedPriceResponseParams)
initWithResponseValue_error mtrCommodityPriceClusterGetDetailedPriceResponseParams responseValue error_ =
  sendOwnedMessage mtrCommodityPriceClusterGetDetailedPriceResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- currentPrice@
currentPrice :: IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> IO (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPrice mtrCommodityPriceClusterGetDetailedPriceResponseParams =
  sendMessage mtrCommodityPriceClusterGetDetailedPriceResponseParams currentPriceSelector

-- | @- setCurrentPrice:@
setCurrentPrice :: (IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams, IsMTRCommodityPriceClusterCommodityPriceStruct value) => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> value -> IO ()
setCurrentPrice mtrCommodityPriceClusterGetDetailedPriceResponseParams value =
  sendMessage mtrCommodityPriceClusterGetDetailedPriceResponseParams setCurrentPriceSelector (toMTRCommodityPriceClusterCommodityPriceStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCommodityPriceClusterGetDetailedPriceResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @currentPrice@
currentPriceSelector :: Selector '[] (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPriceSelector = mkSelector "currentPrice"

-- | @Selector@ for @setCurrentPrice:@
setCurrentPriceSelector :: Selector '[Id MTRCommodityPriceClusterCommodityPriceStruct] ()
setCurrentPriceSelector = mkSelector "setCurrentPrice:"


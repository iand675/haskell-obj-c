{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterGetDetailedForecastResponseParams@.
module ObjC.Matter.MTRCommodityPriceClusterGetDetailedForecastResponseParams
  ( MTRCommodityPriceClusterGetDetailedForecastResponseParams
  , IsMTRCommodityPriceClusterGetDetailedForecastResponseParams(..)
  , initWithResponseValue_error
  , priceForecast
  , setPriceForecast
  , initWithResponseValue_errorSelector
  , priceForecastSelector
  , setPriceForecastSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCommodityPriceClusterGetDetailedForecastResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityPriceClusterGetDetailedForecastResponseParams)
initWithResponseValue_error mtrCommodityPriceClusterGetDetailedForecastResponseParams responseValue error_ =
  sendOwnedMessage mtrCommodityPriceClusterGetDetailedForecastResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- priceForecast@
priceForecast :: IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> IO (Id NSArray)
priceForecast mtrCommodityPriceClusterGetDetailedForecastResponseParams =
  sendMessage mtrCommodityPriceClusterGetDetailedForecastResponseParams priceForecastSelector

-- | @- setPriceForecast:@
setPriceForecast :: (IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams, IsNSArray value) => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> value -> IO ()
setPriceForecast mtrCommodityPriceClusterGetDetailedForecastResponseParams value =
  sendMessage mtrCommodityPriceClusterGetDetailedForecastResponseParams setPriceForecastSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCommodityPriceClusterGetDetailedForecastResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @priceForecast@
priceForecastSelector :: Selector '[] (Id NSArray)
priceForecastSelector = mkSelector "priceForecast"

-- | @Selector@ for @setPriceForecast:@
setPriceForecastSelector :: Selector '[Id NSArray] ()
setPriceForecastSelector = mkSelector "setPriceForecast:"


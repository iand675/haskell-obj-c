{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterGetDayEntryResponseParams@.
module ObjC.Matter.MTRCommodityTariffClusterGetDayEntryResponseParams
  ( MTRCommodityTariffClusterGetDayEntryResponseParams
  , IsMTRCommodityTariffClusterGetDayEntryResponseParams(..)
  , initWithResponseValue_error
  , dayEntry
  , setDayEntry
  , dayEntrySelector
  , initWithResponseValue_errorSelector
  , setDayEntrySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCommodityTariffClusterGetDayEntryResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityTariffClusterGetDayEntryResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityTariffClusterGetDayEntryResponseParams)
initWithResponseValue_error mtrCommodityTariffClusterGetDayEntryResponseParams responseValue error_ =
  sendOwnedMessage mtrCommodityTariffClusterGetDayEntryResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- dayEntry@
dayEntry :: IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams => mtrCommodityTariffClusterGetDayEntryResponseParams -> IO (Id MTRCommodityTariffClusterDayEntryStruct)
dayEntry mtrCommodityTariffClusterGetDayEntryResponseParams =
  sendMessage mtrCommodityTariffClusterGetDayEntryResponseParams dayEntrySelector

-- | @- setDayEntry:@
setDayEntry :: (IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams, IsMTRCommodityTariffClusterDayEntryStruct value) => mtrCommodityTariffClusterGetDayEntryResponseParams -> value -> IO ()
setDayEntry mtrCommodityTariffClusterGetDayEntryResponseParams value =
  sendMessage mtrCommodityTariffClusterGetDayEntryResponseParams setDayEntrySelector (toMTRCommodityTariffClusterDayEntryStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCommodityTariffClusterGetDayEntryResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dayEntry@
dayEntrySelector :: Selector '[] (Id MTRCommodityTariffClusterDayEntryStruct)
dayEntrySelector = mkSelector "dayEntry"

-- | @Selector@ for @setDayEntry:@
setDayEntrySelector :: Selector '[Id MTRCommodityTariffClusterDayEntryStruct] ()
setDayEntrySelector = mkSelector "setDayEntry:"


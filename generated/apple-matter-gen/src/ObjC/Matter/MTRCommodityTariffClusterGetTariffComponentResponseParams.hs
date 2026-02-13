{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterGetTariffComponentResponseParams@.
module ObjC.Matter.MTRCommodityTariffClusterGetTariffComponentResponseParams
  ( MTRCommodityTariffClusterGetTariffComponentResponseParams
  , IsMTRCommodityTariffClusterGetTariffComponentResponseParams(..)
  , initWithResponseValue_error
  , label
  , setLabel
  , dayEntryIDs
  , setDayEntryIDs
  , tariffComponent
  , setTariffComponent
  , dayEntryIDsSelector
  , initWithResponseValue_errorSelector
  , labelSelector
  , setDayEntryIDsSelector
  , setLabelSelector
  , setTariffComponentSelector
  , tariffComponentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCommodityTariffClusterGetTariffComponentResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityTariffClusterGetTariffComponentResponseParams)
initWithResponseValue_error mtrCommodityTariffClusterGetTariffComponentResponseParams responseValue error_ =
  sendOwnedMessage mtrCommodityTariffClusterGetTariffComponentResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- label@
label :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id NSString)
label mtrCommodityTariffClusterGetTariffComponentResponseParams =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSString value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setLabel mtrCommodityTariffClusterGetTariffComponentResponseParams value =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams setLabelSelector (toNSString value)

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterGetTariffComponentResponseParams =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams dayEntryIDsSelector

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSArray value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterGetTariffComponentResponseParams value =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams setDayEntryIDsSelector (toNSArray value)

-- | @- tariffComponent@
tariffComponent :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id MTRCommodityTariffClusterTariffComponentStruct)
tariffComponent mtrCommodityTariffClusterGetTariffComponentResponseParams =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams tariffComponentSelector

-- | @- setTariffComponent:@
setTariffComponent :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsMTRCommodityTariffClusterTariffComponentStruct value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setTariffComponent mtrCommodityTariffClusterGetTariffComponentResponseParams value =
  sendMessage mtrCommodityTariffClusterGetTariffComponentResponseParams setTariffComponentSelector (toMTRCommodityTariffClusterTariffComponentStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCommodityTariffClusterGetTariffComponentResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector '[] (Id NSArray)
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector '[Id NSArray] ()
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

-- | @Selector@ for @tariffComponent@
tariffComponentSelector :: Selector '[] (Id MTRCommodityTariffClusterTariffComponentStruct)
tariffComponentSelector = mkSelector "tariffComponent"

-- | @Selector@ for @setTariffComponent:@
setTariffComponentSelector :: Selector '[Id MTRCommodityTariffClusterTariffComponentStruct] ()
setTariffComponentSelector = mkSelector "setTariffComponent:"


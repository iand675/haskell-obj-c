{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAddThermostatSuggestionResponseParams@.
module ObjC.Matter.MTRThermostatClusterAddThermostatSuggestionResponseParams
  ( MTRThermostatClusterAddThermostatSuggestionResponseParams
  , IsMTRThermostatClusterAddThermostatSuggestionResponseParams(..)
  , initWithResponseValue_error
  , uniqueID
  , setUniqueID
  , initWithResponseValue_errorSelector
  , setUniqueIDSelector
  , uniqueIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRThermostatClusterAddThermostatSuggestionResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterAddThermostatSuggestionResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterAddThermostatSuggestionResponseParams)
initWithResponseValue_error mtrThermostatClusterAddThermostatSuggestionResponseParams responseValue error_ =
  sendOwnedMessage mtrThermostatClusterAddThermostatSuggestionResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- uniqueID@
uniqueID :: IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams => mtrThermostatClusterAddThermostatSuggestionResponseParams -> IO (Id NSNumber)
uniqueID mtrThermostatClusterAddThermostatSuggestionResponseParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionResponseParams uniqueIDSelector

-- | @- setUniqueID:@
setUniqueID :: (IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionResponseParams -> value -> IO ()
setUniqueID mtrThermostatClusterAddThermostatSuggestionResponseParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionResponseParams setUniqueIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRThermostatClusterAddThermostatSuggestionResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] (Id NSNumber)
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector '[Id NSNumber] ()
setUniqueIDSelector = mkSelector "setUniqueID:"


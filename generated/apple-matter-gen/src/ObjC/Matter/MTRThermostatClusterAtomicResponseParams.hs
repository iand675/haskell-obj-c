{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAtomicResponseParams@.
module ObjC.Matter.MTRThermostatClusterAtomicResponseParams
  ( MTRThermostatClusterAtomicResponseParams
  , IsMTRThermostatClusterAtomicResponseParams(..)
  , initWithResponseValue_error
  , statusCode
  , setStatusCode
  , attributeStatus
  , setAttributeStatus
  , timeout
  , setTimeout
  , attributeStatusSelector
  , initWithResponseValue_errorSelector
  , setAttributeStatusSelector
  , setStatusCodeSelector
  , setTimeoutSelector
  , statusCodeSelector
  , timeoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRThermostatClusterAtomicResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterAtomicResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterAtomicResponseParams)
initWithResponseValue_error mtrThermostatClusterAtomicResponseParams responseValue error_ =
  sendOwnedMessage mtrThermostatClusterAtomicResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- statusCode@
statusCode :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSNumber)
statusCode mtrThermostatClusterAtomicResponseParams =
  sendMessage mtrThermostatClusterAtomicResponseParams statusCodeSelector

-- | @- setStatusCode:@
setStatusCode :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSNumber value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setStatusCode mtrThermostatClusterAtomicResponseParams value =
  sendMessage mtrThermostatClusterAtomicResponseParams setStatusCodeSelector (toNSNumber value)

-- | @- attributeStatus@
attributeStatus :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSArray)
attributeStatus mtrThermostatClusterAtomicResponseParams =
  sendMessage mtrThermostatClusterAtomicResponseParams attributeStatusSelector

-- | @- setAttributeStatus:@
setAttributeStatus :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSArray value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setAttributeStatus mtrThermostatClusterAtomicResponseParams value =
  sendMessage mtrThermostatClusterAtomicResponseParams setAttributeStatusSelector (toNSArray value)

-- | @- timeout@
timeout :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSNumber)
timeout mtrThermostatClusterAtomicResponseParams =
  sendMessage mtrThermostatClusterAtomicResponseParams timeoutSelector

-- | @- setTimeout:@
setTimeout :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSNumber value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setTimeout mtrThermostatClusterAtomicResponseParams value =
  sendMessage mtrThermostatClusterAtomicResponseParams setTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRThermostatClusterAtomicResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] (Id NSNumber)
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector '[Id NSNumber] ()
setStatusCodeSelector = mkSelector "setStatusCode:"

-- | @Selector@ for @attributeStatus@
attributeStatusSelector :: Selector '[] (Id NSArray)
attributeStatusSelector = mkSelector "attributeStatus"

-- | @Selector@ for @setAttributeStatus:@
setAttributeStatusSelector :: Selector '[Id NSArray] ()
setAttributeStatusSelector = mkSelector "setAttributeStatus:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector '[] (Id NSNumber)
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector '[Id NSNumber] ()
setTimeoutSelector = mkSelector "setTimeout:"


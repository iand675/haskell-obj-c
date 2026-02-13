{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterStringEchoResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterStringEchoResponseParams
  ( MTRUnitTestingClusterStringEchoResponseParams
  , IsMTRUnitTestingClusterStringEchoResponseParams(..)
  , initWithResponseValue_error
  , payload
  , setPayload
  , initWithResponseValue_errorSelector
  , payloadSelector
  , setPayloadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterStringEchoResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterStringEchoResponseParams mtrUnitTestingClusterStringEchoResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterStringEchoResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterStringEchoResponseParams)
initWithResponseValue_error mtrUnitTestingClusterStringEchoResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterStringEchoResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- payload@
payload :: IsMTRUnitTestingClusterStringEchoResponseParams mtrUnitTestingClusterStringEchoResponseParams => mtrUnitTestingClusterStringEchoResponseParams -> IO (Id NSData)
payload mtrUnitTestingClusterStringEchoResponseParams =
  sendMessage mtrUnitTestingClusterStringEchoResponseParams payloadSelector

-- | @- setPayload:@
setPayload :: (IsMTRUnitTestingClusterStringEchoResponseParams mtrUnitTestingClusterStringEchoResponseParams, IsNSData value) => mtrUnitTestingClusterStringEchoResponseParams -> value -> IO ()
setPayload mtrUnitTestingClusterStringEchoResponseParams value =
  sendMessage mtrUnitTestingClusterStringEchoResponseParams setPayloadSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterStringEchoResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @payload@
payloadSelector :: Selector '[] (Id NSData)
payloadSelector = mkSelector "payload"

-- | @Selector@ for @setPayload:@
setPayloadSelector :: Selector '[Id NSData] ()
setPayloadSelector = mkSelector "setPayload:"


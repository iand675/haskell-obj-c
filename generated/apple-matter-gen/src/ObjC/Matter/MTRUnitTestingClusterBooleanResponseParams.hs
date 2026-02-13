{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterBooleanResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterBooleanResponseParams
  ( MTRUnitTestingClusterBooleanResponseParams
  , IsMTRUnitTestingClusterBooleanResponseParams(..)
  , initWithResponseValue_error
  , value
  , setValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , setTimedInvokeTimeoutMsSelector
  , setValueSelector
  , timedInvokeTimeoutMsSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterBooleanResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterBooleanResponseParams mtrUnitTestingClusterBooleanResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterBooleanResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterBooleanResponseParams)
initWithResponseValue_error mtrUnitTestingClusterBooleanResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterBooleanResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- value@
value :: IsMTRUnitTestingClusterBooleanResponseParams mtrUnitTestingClusterBooleanResponseParams => mtrUnitTestingClusterBooleanResponseParams -> IO (Id NSNumber)
value mtrUnitTestingClusterBooleanResponseParams =
  sendMessage mtrUnitTestingClusterBooleanResponseParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRUnitTestingClusterBooleanResponseParams mtrUnitTestingClusterBooleanResponseParams, IsNSNumber value) => mtrUnitTestingClusterBooleanResponseParams -> value -> IO ()
setValue mtrUnitTestingClusterBooleanResponseParams value =
  sendMessage mtrUnitTestingClusterBooleanResponseParams setValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterBooleanResponseParams mtrUnitTestingClusterBooleanResponseParams => mtrUnitTestingClusterBooleanResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterBooleanResponseParams =
  sendMessage mtrUnitTestingClusterBooleanResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterBooleanResponseParams mtrUnitTestingClusterBooleanResponseParams, IsNSNumber value) => mtrUnitTestingClusterBooleanResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterBooleanResponseParams value =
  sendMessage mtrUnitTestingClusterBooleanResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterBooleanResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


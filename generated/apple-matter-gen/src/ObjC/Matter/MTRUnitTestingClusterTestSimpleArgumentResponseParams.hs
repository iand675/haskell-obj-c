{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestSimpleArgumentResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestSimpleArgumentResponseParams
  ( MTRUnitTestingClusterTestSimpleArgumentResponseParams
  , IsMTRUnitTestingClusterTestSimpleArgumentResponseParams(..)
  , initWithResponseValue_error
  , returnValue
  , setReturnValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , returnValueSelector
  , setReturnValueSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterTestSimpleArgumentResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestSimpleArgumentResponseParams mtrUnitTestingClusterTestSimpleArgumentResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestSimpleArgumentResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestSimpleArgumentResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestSimpleArgumentResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestSimpleArgumentResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- returnValue@
returnValue :: IsMTRUnitTestingClusterTestSimpleArgumentResponseParams mtrUnitTestingClusterTestSimpleArgumentResponseParams => mtrUnitTestingClusterTestSimpleArgumentResponseParams -> IO (Id NSNumber)
returnValue mtrUnitTestingClusterTestSimpleArgumentResponseParams =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentResponseParams returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: (IsMTRUnitTestingClusterTestSimpleArgumentResponseParams mtrUnitTestingClusterTestSimpleArgumentResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestSimpleArgumentResponseParams -> value -> IO ()
setReturnValue mtrUnitTestingClusterTestSimpleArgumentResponseParams value =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentResponseParams setReturnValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestSimpleArgumentResponseParams mtrUnitTestingClusterTestSimpleArgumentResponseParams => mtrUnitTestingClusterTestSimpleArgumentResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestSimpleArgumentResponseParams =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestSimpleArgumentResponseParams mtrUnitTestingClusterTestSimpleArgumentResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestSimpleArgumentResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestSimpleArgumentResponseParams value =
  sendMessage mtrUnitTestingClusterTestSimpleArgumentResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestSimpleArgumentResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector '[] (Id NSNumber)
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector '[Id NSNumber] ()
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


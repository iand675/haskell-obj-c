{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestSpecificResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestSpecificResponseParams
  ( MTRUnitTestingClusterTestSpecificResponseParams
  , IsMTRUnitTestingClusterTestSpecificResponseParams(..)
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

-- | Initialize an MTRUnitTestingClusterTestSpecificResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestSpecificResponseParams mtrUnitTestingClusterTestSpecificResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestSpecificResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestSpecificResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestSpecificResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestSpecificResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- returnValue@
returnValue :: IsMTRUnitTestingClusterTestSpecificResponseParams mtrUnitTestingClusterTestSpecificResponseParams => mtrUnitTestingClusterTestSpecificResponseParams -> IO (Id NSNumber)
returnValue mtrUnitTestingClusterTestSpecificResponseParams =
  sendMessage mtrUnitTestingClusterTestSpecificResponseParams returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: (IsMTRUnitTestingClusterTestSpecificResponseParams mtrUnitTestingClusterTestSpecificResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestSpecificResponseParams -> value -> IO ()
setReturnValue mtrUnitTestingClusterTestSpecificResponseParams value =
  sendMessage mtrUnitTestingClusterTestSpecificResponseParams setReturnValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestSpecificResponseParams mtrUnitTestingClusterTestSpecificResponseParams => mtrUnitTestingClusterTestSpecificResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestSpecificResponseParams =
  sendMessage mtrUnitTestingClusterTestSpecificResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestSpecificResponseParams mtrUnitTestingClusterTestSpecificResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestSpecificResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestSpecificResponseParams value =
  sendMessage mtrUnitTestingClusterTestSpecificResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestSpecificResponseParams)
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


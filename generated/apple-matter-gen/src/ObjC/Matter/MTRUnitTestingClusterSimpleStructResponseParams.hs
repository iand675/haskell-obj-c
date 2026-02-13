{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterSimpleStructResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterSimpleStructResponseParams
  ( MTRUnitTestingClusterSimpleStructResponseParams
  , IsMTRUnitTestingClusterSimpleStructResponseParams(..)
  , initWithResponseValue_error
  , arg1
  , setArg1
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , arg1Selector
  , initWithResponseValue_errorSelector
  , setArg1Selector
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

-- | Initialize an MTRUnitTestingClusterSimpleStructResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterSimpleStructResponseParams mtrUnitTestingClusterSimpleStructResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterSimpleStructResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterSimpleStructResponseParams)
initWithResponseValue_error mtrUnitTestingClusterSimpleStructResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterSimpleStructResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterSimpleStructResponseParams mtrUnitTestingClusterSimpleStructResponseParams => mtrUnitTestingClusterSimpleStructResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
arg1 mtrUnitTestingClusterSimpleStructResponseParams =
  sendMessage mtrUnitTestingClusterSimpleStructResponseParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterSimpleStructResponseParams mtrUnitTestingClusterSimpleStructResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterSimpleStructResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterSimpleStructResponseParams value =
  sendMessage mtrUnitTestingClusterSimpleStructResponseParams setArg1Selector (toMTRUnitTestingClusterSimpleStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterSimpleStructResponseParams mtrUnitTestingClusterSimpleStructResponseParams => mtrUnitTestingClusterSimpleStructResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterSimpleStructResponseParams =
  sendMessage mtrUnitTestingClusterSimpleStructResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterSimpleStructResponseParams mtrUnitTestingClusterSimpleStructResponseParams, IsNSNumber value) => mtrUnitTestingClusterSimpleStructResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterSimpleStructResponseParams value =
  sendMessage mtrUnitTestingClusterSimpleStructResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterSimpleStructResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


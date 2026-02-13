{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestEmitTestEventResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestEmitTestEventResponseParams
  ( MTRUnitTestingClusterTestEmitTestEventResponseParams
  , IsMTRUnitTestingClusterTestEmitTestEventResponseParams(..)
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

-- | Initialize an MTRUnitTestingClusterTestEmitTestEventResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestEmitTestEventResponseParams mtrUnitTestingClusterTestEmitTestEventResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestEmitTestEventResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestEmitTestEventResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestEmitTestEventResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestEmitTestEventResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- value@
value :: IsMTRUnitTestingClusterTestEmitTestEventResponseParams mtrUnitTestingClusterTestEmitTestEventResponseParams => mtrUnitTestingClusterTestEmitTestEventResponseParams -> IO (Id NSNumber)
value mtrUnitTestingClusterTestEmitTestEventResponseParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventResponseParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRUnitTestingClusterTestEmitTestEventResponseParams mtrUnitTestingClusterTestEmitTestEventResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventResponseParams -> value -> IO ()
setValue mtrUnitTestingClusterTestEmitTestEventResponseParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventResponseParams setValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestEmitTestEventResponseParams mtrUnitTestingClusterTestEmitTestEventResponseParams => mtrUnitTestingClusterTestEmitTestEventResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestEmitTestEventResponseParams =
  sendMessage mtrUnitTestingClusterTestEmitTestEventResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestEmitTestEventResponseParams mtrUnitTestingClusterTestEmitTestEventResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestEmitTestEventResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestEmitTestEventResponseParams value =
  sendMessage mtrUnitTestingClusterTestEmitTestEventResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestEmitTestEventResponseParams)
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


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRKeypadInputClusterSendKeyResponseParams@.
module ObjC.Matter.MTRKeypadInputClusterSendKeyResponseParams
  ( MTRKeypadInputClusterSendKeyResponseParams
  , IsMTRKeypadInputClusterSendKeyResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , statusSelector
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

-- | Initialize an MTRKeypadInputClusterSendKeyResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRKeypadInputClusterSendKeyResponseParams mtrKeypadInputClusterSendKeyResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrKeypadInputClusterSendKeyResponseParams -> responseValue -> error_ -> IO (Id MTRKeypadInputClusterSendKeyResponseParams)
initWithResponseValue_error mtrKeypadInputClusterSendKeyResponseParams responseValue error_ =
  sendOwnedMessage mtrKeypadInputClusterSendKeyResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRKeypadInputClusterSendKeyResponseParams mtrKeypadInputClusterSendKeyResponseParams => mtrKeypadInputClusterSendKeyResponseParams -> IO (Id NSNumber)
status mtrKeypadInputClusterSendKeyResponseParams =
  sendMessage mtrKeypadInputClusterSendKeyResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRKeypadInputClusterSendKeyResponseParams mtrKeypadInputClusterSendKeyResponseParams, IsNSNumber value) => mtrKeypadInputClusterSendKeyResponseParams -> value -> IO ()
setStatus mtrKeypadInputClusterSendKeyResponseParams value =
  sendMessage mtrKeypadInputClusterSendKeyResponseParams setStatusSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRKeypadInputClusterSendKeyResponseParams mtrKeypadInputClusterSendKeyResponseParams => mtrKeypadInputClusterSendKeyResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrKeypadInputClusterSendKeyResponseParams =
  sendMessage mtrKeypadInputClusterSendKeyResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRKeypadInputClusterSendKeyResponseParams mtrKeypadInputClusterSendKeyResponseParams, IsNSNumber value) => mtrKeypadInputClusterSendKeyResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrKeypadInputClusterSendKeyResponseParams value =
  sendMessage mtrKeypadInputClusterSendKeyResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRKeypadInputClusterSendKeyResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


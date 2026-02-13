{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterCommissioningCompleteResponseParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterCommissioningCompleteResponseParams
  ( MTRGeneralCommissioningClusterCommissioningCompleteResponseParams
  , IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams(..)
  , initWithResponseValue_error
  , errorCode
  , setErrorCode
  , debugText
  , setDebugText
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , debugTextSelector
  , errorCodeSelector
  , initWithResponseValue_errorSelector
  , setDebugTextSelector
  , setErrorCodeSelector
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

-- | Initialize an MTRGeneralCommissioningClusterCommissioningCompleteResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralCommissioningClusterCommissioningCompleteResponseParams)
initWithResponseValue_error mtrGeneralCommissioningClusterCommissioningCompleteResponseParams responseValue error_ =
  sendOwnedMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- errorCode@
errorCode :: IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> IO (Id NSNumber)
errorCode mtrGeneralCommissioningClusterCommissioningCompleteResponseParams =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams errorCodeSelector

-- | @- setErrorCode:@
setErrorCode :: (IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams, IsNSNumber value) => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> value -> IO ()
setErrorCode mtrGeneralCommissioningClusterCommissioningCompleteResponseParams value =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams setErrorCodeSelector (toNSNumber value)

-- | @- debugText@
debugText :: IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> IO (Id NSString)
debugText mtrGeneralCommissioningClusterCommissioningCompleteResponseParams =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams debugTextSelector

-- | @- setDebugText:@
setDebugText :: (IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams, IsNSString value) => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> value -> IO ()
setDebugText mtrGeneralCommissioningClusterCommissioningCompleteResponseParams value =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams setDebugTextSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralCommissioningClusterCommissioningCompleteResponseParams =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralCommissioningClusterCommissioningCompleteResponseParams mtrGeneralCommissioningClusterCommissioningCompleteResponseParams, IsNSNumber value) => mtrGeneralCommissioningClusterCommissioningCompleteResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralCommissioningClusterCommissioningCompleteResponseParams value =
  sendMessage mtrGeneralCommissioningClusterCommissioningCompleteResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGeneralCommissioningClusterCommissioningCompleteResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @errorCode@
errorCodeSelector :: Selector '[] (Id NSNumber)
errorCodeSelector = mkSelector "errorCode"

-- | @Selector@ for @setErrorCode:@
setErrorCodeSelector :: Selector '[Id NSNumber] ()
setErrorCodeSelector = mkSelector "setErrorCode:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector '[] (Id NSString)
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector '[Id NSString] ()
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


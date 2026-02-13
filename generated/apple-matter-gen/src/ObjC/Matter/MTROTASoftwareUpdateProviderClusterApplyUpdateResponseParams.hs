{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams@.
module ObjC.Matter.MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams
  ( MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams
  , IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams(..)
  , initWithResponseValue_error
  , action
  , setAction
  , delayedActionTime
  , setDelayedActionTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , actionSelector
  , delayedActionTimeSelector
  , initWithResponseValue_errorSelector
  , setActionSelector
  , setDelayedActionTimeSelector
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

-- | Initialize an MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> responseValue -> error_ -> IO (Id MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams)
initWithResponseValue_error mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams responseValue error_ =
  sendOwnedMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- action@
action :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
action mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams actionSelector

-- | @- setAction:@
setAction :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setAction mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams setActionSelector (toNSNumber value)

-- | @- delayedActionTime@
delayedActionTime :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
delayedActionTime mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams delayedActionTimeSelector

-- | @- setDelayedActionTime:@
setDelayedActionTime :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setDelayedActionTime mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams setDelayedActionTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROTASoftwareUpdateProviderClusterApplyUpdateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] (Id NSNumber)
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Id NSNumber] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @delayedActionTime@
delayedActionTimeSelector :: Selector '[] (Id NSNumber)
delayedActionTimeSelector = mkSelector "delayedActionTime"

-- | @Selector@ for @setDelayedActionTime:@
setDelayedActionTimeSelector :: Selector '[Id NSNumber] ()
setDelayedActionTimeSelector = mkSelector "setDelayedActionTime:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


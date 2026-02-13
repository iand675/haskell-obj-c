{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChangeChannelResponseParams@.
module ObjC.Matter.MTRChannelClusterChangeChannelResponseParams
  ( MTRChannelClusterChangeChannelResponseParams
  , IsMTRChannelClusterChangeChannelResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , dataSelector
  , initWithResponseValue_errorSelector
  , setDataSelector
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

-- | Initialize an MTRChannelClusterChangeChannelResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrChannelClusterChangeChannelResponseParams -> responseValue -> error_ -> IO (Id MTRChannelClusterChangeChannelResponseParams)
initWithResponseValue_error mtrChannelClusterChangeChannelResponseParams responseValue error_ =
  sendOwnedMessage mtrChannelClusterChangeChannelResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams => mtrChannelClusterChangeChannelResponseParams -> IO (Id NSNumber)
status mtrChannelClusterChangeChannelResponseParams =
  sendMessage mtrChannelClusterChangeChannelResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams, IsNSNumber value) => mtrChannelClusterChangeChannelResponseParams -> value -> IO ()
setStatus mtrChannelClusterChangeChannelResponseParams value =
  sendMessage mtrChannelClusterChangeChannelResponseParams setStatusSelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams => mtrChannelClusterChangeChannelResponseParams -> IO (Id NSString)
data_ mtrChannelClusterChangeChannelResponseParams =
  sendMessage mtrChannelClusterChangeChannelResponseParams dataSelector

-- | @- setData:@
setData :: (IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams, IsNSString value) => mtrChannelClusterChangeChannelResponseParams -> value -> IO ()
setData mtrChannelClusterChangeChannelResponseParams value =
  sendMessage mtrChannelClusterChangeChannelResponseParams setDataSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams => mtrChannelClusterChangeChannelResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterChangeChannelResponseParams =
  sendMessage mtrChannelClusterChangeChannelResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterChangeChannelResponseParams mtrChannelClusterChangeChannelResponseParams, IsNSNumber value) => mtrChannelClusterChangeChannelResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterChangeChannelResponseParams value =
  sendMessage mtrChannelClusterChangeChannelResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRChannelClusterChangeChannelResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


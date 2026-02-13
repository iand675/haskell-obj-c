{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterKeySetReadResponseParams@.
module ObjC.Matter.MTRGroupKeyManagementClusterKeySetReadResponseParams
  ( MTRGroupKeyManagementClusterKeySetReadResponseParams
  , IsMTRGroupKeyManagementClusterKeySetReadResponseParams(..)
  , initWithResponseValue_error
  , groupKeySet
  , setGroupKeySet
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , groupKeySetSelector
  , initWithResponseValue_errorSelector
  , setGroupKeySetSelector
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

-- | Initialize an MTRGroupKeyManagementClusterKeySetReadResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupKeyManagementClusterKeySetReadResponseParams mtrGroupKeyManagementClusterKeySetReadResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupKeyManagementClusterKeySetReadResponseParams -> responseValue -> error_ -> IO (Id MTRGroupKeyManagementClusterKeySetReadResponseParams)
initWithResponseValue_error mtrGroupKeyManagementClusterKeySetReadResponseParams responseValue error_ =
  sendOwnedMessage mtrGroupKeyManagementClusterKeySetReadResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- groupKeySet@
groupKeySet :: IsMTRGroupKeyManagementClusterKeySetReadResponseParams mtrGroupKeyManagementClusterKeySetReadResponseParams => mtrGroupKeyManagementClusterKeySetReadResponseParams -> IO (Id MTRGroupKeyManagementClusterGroupKeySetStruct)
groupKeySet mtrGroupKeyManagementClusterKeySetReadResponseParams =
  sendMessage mtrGroupKeyManagementClusterKeySetReadResponseParams groupKeySetSelector

-- | @- setGroupKeySet:@
setGroupKeySet :: (IsMTRGroupKeyManagementClusterKeySetReadResponseParams mtrGroupKeyManagementClusterKeySetReadResponseParams, IsMTRGroupKeyManagementClusterGroupKeySetStruct value) => mtrGroupKeyManagementClusterKeySetReadResponseParams -> value -> IO ()
setGroupKeySet mtrGroupKeyManagementClusterKeySetReadResponseParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetReadResponseParams setGroupKeySetSelector (toMTRGroupKeyManagementClusterGroupKeySetStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupKeyManagementClusterKeySetReadResponseParams mtrGroupKeyManagementClusterKeySetReadResponseParams => mtrGroupKeyManagementClusterKeySetReadResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetReadResponseParams =
  sendMessage mtrGroupKeyManagementClusterKeySetReadResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupKeyManagementClusterKeySetReadResponseParams mtrGroupKeyManagementClusterKeySetReadResponseParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetReadResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetReadResponseParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetReadResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGroupKeyManagementClusterKeySetReadResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @groupKeySet@
groupKeySetSelector :: Selector '[] (Id MTRGroupKeyManagementClusterGroupKeySetStruct)
groupKeySetSelector = mkSelector "groupKeySet"

-- | @Selector@ for @setGroupKeySet:@
setGroupKeySetSelector :: Selector '[Id MTRGroupKeyManagementClusterGroupKeySetStruct] ()
setGroupKeySetSelector = mkSelector "setGroupKeySet:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


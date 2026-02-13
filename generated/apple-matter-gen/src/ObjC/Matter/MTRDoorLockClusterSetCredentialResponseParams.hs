{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetCredentialResponseParams@.
module ObjC.Matter.MTRDoorLockClusterSetCredentialResponseParams
  ( MTRDoorLockClusterSetCredentialResponseParams
  , IsMTRDoorLockClusterSetCredentialResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , userIndex
  , setUserIndex
  , nextCredentialIndex
  , setNextCredentialIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , nextCredentialIndexSelector
  , setNextCredentialIndexSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , statusSelector
  , timedInvokeTimeoutMsSelector
  , userIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRDoorLockClusterSetCredentialResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterSetCredentialResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterSetCredentialResponseParams)
initWithResponseValue_error mtrDoorLockClusterSetCredentialResponseParams responseValue error_ =
  sendOwnedMessage mtrDoorLockClusterSetCredentialResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterSetCredentialResponseParams =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterSetCredentialResponseParams value =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams setStatusSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetCredentialResponseParams =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetCredentialResponseParams value =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams setUserIndexSelector (toNSNumber value)

-- | @- nextCredentialIndex@
nextCredentialIndex :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
nextCredentialIndex mtrDoorLockClusterSetCredentialResponseParams =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams nextCredentialIndexSelector

-- | @- setNextCredentialIndex:@
setNextCredentialIndex :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setNextCredentialIndex mtrDoorLockClusterSetCredentialResponseParams value =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams setNextCredentialIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetCredentialResponseParams =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetCredentialResponseParams value =
  sendMessage mtrDoorLockClusterSetCredentialResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDoorLockClusterSetCredentialResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @nextCredentialIndex@
nextCredentialIndexSelector :: Selector '[] (Id NSNumber)
nextCredentialIndexSelector = mkSelector "nextCredentialIndex"

-- | @Selector@ for @setNextCredentialIndex:@
setNextCredentialIndexSelector :: Selector '[Id NSNumber] ()
setNextCredentialIndexSelector = mkSelector "setNextCredentialIndex:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"


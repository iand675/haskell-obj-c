{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetCredentialParams@.
module ObjC.Matter.MTRDoorLockClusterSetCredentialParams
  ( MTRDoorLockClusterSetCredentialParams
  , IsMTRDoorLockClusterSetCredentialParams(..)
  , operationType
  , setOperationType
  , credential
  , setCredential
  , credentialData
  , setCredentialData
  , userIndex
  , setUserIndex
  , userStatus
  , setUserStatus
  , userType
  , setUserType
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , credentialDataSelector
  , credentialSelector
  , operationTypeSelector
  , serverSideProcessingTimeoutSelector
  , setCredentialDataSelector
  , setCredentialSelector
  , setOperationTypeSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setUserStatusSelector
  , setUserTypeSelector
  , timedInvokeTimeoutMsSelector
  , userIndexSelector
  , userStatusSelector
  , userTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- operationType@
operationType :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams operationTypeSelector

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setOperationType mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setOperationTypeSelector (toNSNumber value)

-- | @- credential@
credential :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id MTRDoorLockClusterCredentialStruct)
credential mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams credentialSelector

-- | @- setCredential:@
setCredential :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsMTRDoorLockClusterCredentialStruct value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setCredential mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setCredentialSelector (toMTRDoorLockClusterCredentialStruct value)

-- | @- credentialData@
credentialData :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSData)
credentialData mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams credentialDataSelector

-- | @- setCredentialData:@
setCredentialData :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSData value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setCredentialData mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setCredentialDataSelector (toNSData value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setUserIndexSelector (toNSNumber value)

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams userStatusSelector

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserStatus mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setUserStatusSelector (toNSNumber value)

-- | @- userType@
userType :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
userType mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams userTypeSelector

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setUserType mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setUserTypeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams => mtrDoorLockClusterSetCredentialParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetCredentialParams =
  sendMessage mtrDoorLockClusterSetCredentialParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetCredentialParams mtrDoorLockClusterSetCredentialParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetCredentialParams value =
  sendMessage mtrDoorLockClusterSetCredentialParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationType@
operationTypeSelector :: Selector '[] (Id NSNumber)
operationTypeSelector = mkSelector "operationType"

-- | @Selector@ for @setOperationType:@
setOperationTypeSelector :: Selector '[Id NSNumber] ()
setOperationTypeSelector = mkSelector "setOperationType:"

-- | @Selector@ for @credential@
credentialSelector :: Selector '[] (Id MTRDoorLockClusterCredentialStruct)
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector '[Id MTRDoorLockClusterCredentialStruct] ()
setCredentialSelector = mkSelector "setCredential:"

-- | @Selector@ for @credentialData@
credentialDataSelector :: Selector '[] (Id NSData)
credentialDataSelector = mkSelector "credentialData"

-- | @Selector@ for @setCredentialData:@
setCredentialDataSelector :: Selector '[Id NSData] ()
setCredentialDataSelector = mkSelector "setCredentialData:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @userStatus@
userStatusSelector :: Selector '[] (Id NSNumber)
userStatusSelector = mkSelector "userStatus"

-- | @Selector@ for @setUserStatus:@
setUserStatusSelector :: Selector '[Id NSNumber] ()
setUserStatusSelector = mkSelector "setUserStatus:"

-- | @Selector@ for @userType@
userTypeSelector :: Selector '[] (Id NSNumber)
userTypeSelector = mkSelector "userType"

-- | @Selector@ for @setUserType:@
setUserTypeSelector :: Selector '[Id NSNumber] ()
setUserTypeSelector = mkSelector "setUserType:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleSetAliroCredentialParams@.
module ObjC.Matter.MTRDoorLockClusterAppleSetAliroCredentialParams
  ( MTRDoorLockClusterAppleSetAliroCredentialParams
  , IsMTRDoorLockClusterAppleSetAliroCredentialParams(..)
  , operationType
  , setOperationType
  , credential
  , setCredential
  , credentialData
  , setCredentialData
  , userIndex
  , setUserIndex
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

-- | @- operationType@
operationType :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams operationTypeSelector

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setOperationType mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setOperationTypeSelector (toNSNumber value)

-- | @- credential@
credential :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id MTRDoorLockClusterAppleAliroCredentialStruct)
credential mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams credentialSelector

-- | @- setCredential:@
setCredential :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsMTRDoorLockClusterAppleAliroCredentialStruct value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setCredential mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setCredentialSelector (toMTRDoorLockClusterAppleAliroCredentialStruct value)

-- | @- credentialData@
credentialData :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSData)
credentialData mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams credentialDataSelector

-- | @- setCredentialData:@
setCredentialData :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSData value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setCredentialData mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setCredentialDataSelector (toNSData value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setUserIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams => mtrDoorLockClusterAppleSetAliroCredentialParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterAppleSetAliroCredentialParams =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterAppleSetAliroCredentialParams mtrDoorLockClusterAppleSetAliroCredentialParams, IsNSNumber value) => mtrDoorLockClusterAppleSetAliroCredentialParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterAppleSetAliroCredentialParams value =
  sendMessage mtrDoorLockClusterAppleSetAliroCredentialParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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
credentialSelector :: Selector '[] (Id MTRDoorLockClusterAppleAliroCredentialStruct)
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector '[Id MTRDoorLockClusterAppleAliroCredentialStruct] ()
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


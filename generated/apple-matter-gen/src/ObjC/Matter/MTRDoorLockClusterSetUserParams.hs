{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetUserParams@.
module ObjC.Matter.MTRDoorLockClusterSetUserParams
  ( MTRDoorLockClusterSetUserParams
  , IsMTRDoorLockClusterSetUserParams(..)
  , operationType
  , setOperationType
  , userIndex
  , setUserIndex
  , userName
  , setUserName
  , userUniqueID
  , setUserUniqueID
  , userStatus
  , setUserStatus
  , userType
  , setUserType
  , credentialRule
  , setCredentialRule
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , userUniqueId
  , setUserUniqueId
  , credentialRuleSelector
  , operationTypeSelector
  , serverSideProcessingTimeoutSelector
  , setCredentialRuleSelector
  , setOperationTypeSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUserIndexSelector
  , setUserNameSelector
  , setUserStatusSelector
  , setUserTypeSelector
  , setUserUniqueIDSelector
  , setUserUniqueIdSelector
  , timedInvokeTimeoutMsSelector
  , userIndexSelector
  , userNameSelector
  , userStatusSelector
  , userTypeSelector
  , userUniqueIDSelector
  , userUniqueIdSelector


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
operationType :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
operationType mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams operationTypeSelector

-- | @- setOperationType:@
setOperationType :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setOperationType mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setOperationTypeSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserIndexSelector (toNSNumber value)

-- | @- userName@
userName :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSString)
userName mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userNameSelector

-- | @- setUserName:@
setUserName :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSString value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserName mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserNameSelector (toNSString value)

-- | @- userUniqueID@
userUniqueID :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userUniqueID mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userUniqueIDSelector

-- | @- setUserUniqueID:@
setUserUniqueID :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserUniqueID mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserUniqueIDSelector (toNSNumber value)

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userStatusSelector

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserStatus mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserStatusSelector (toNSNumber value)

-- | @- userType@
userType :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userType mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userTypeSelector

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserType mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserTypeSelector (toNSNumber value)

-- | @- credentialRule@
credentialRule :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
credentialRule mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams credentialRuleSelector

-- | @- setCredentialRule:@
setCredentialRule :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setCredentialRule mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setCredentialRuleSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- | @- userUniqueId@
userUniqueId :: IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams => mtrDoorLockClusterSetUserParams -> IO (Id NSNumber)
userUniqueId mtrDoorLockClusterSetUserParams =
  sendMessage mtrDoorLockClusterSetUserParams userUniqueIdSelector

-- | @- setUserUniqueId:@
setUserUniqueId :: (IsMTRDoorLockClusterSetUserParams mtrDoorLockClusterSetUserParams, IsNSNumber value) => mtrDoorLockClusterSetUserParams -> value -> IO ()
setUserUniqueId mtrDoorLockClusterSetUserParams value =
  sendMessage mtrDoorLockClusterSetUserParams setUserUniqueIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationType@
operationTypeSelector :: Selector '[] (Id NSNumber)
operationTypeSelector = mkSelector "operationType"

-- | @Selector@ for @setOperationType:@
setOperationTypeSelector :: Selector '[Id NSNumber] ()
setOperationTypeSelector = mkSelector "setOperationType:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @userName@
userNameSelector :: Selector '[] (Id NSString)
userNameSelector = mkSelector "userName"

-- | @Selector@ for @setUserName:@
setUserNameSelector :: Selector '[Id NSString] ()
setUserNameSelector = mkSelector "setUserName:"

-- | @Selector@ for @userUniqueID@
userUniqueIDSelector :: Selector '[] (Id NSNumber)
userUniqueIDSelector = mkSelector "userUniqueID"

-- | @Selector@ for @setUserUniqueID:@
setUserUniqueIDSelector :: Selector '[Id NSNumber] ()
setUserUniqueIDSelector = mkSelector "setUserUniqueID:"

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

-- | @Selector@ for @credentialRule@
credentialRuleSelector :: Selector '[] (Id NSNumber)
credentialRuleSelector = mkSelector "credentialRule"

-- | @Selector@ for @setCredentialRule:@
setCredentialRuleSelector :: Selector '[Id NSNumber] ()
setCredentialRuleSelector = mkSelector "setCredentialRule:"

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

-- | @Selector@ for @userUniqueId@
userUniqueIdSelector :: Selector '[] (Id NSNumber)
userUniqueIdSelector = mkSelector "userUniqueId"

-- | @Selector@ for @setUserUniqueId:@
setUserUniqueIdSelector :: Selector '[Id NSNumber] ()
setUserUniqueIdSelector = mkSelector "setUserUniqueId:"


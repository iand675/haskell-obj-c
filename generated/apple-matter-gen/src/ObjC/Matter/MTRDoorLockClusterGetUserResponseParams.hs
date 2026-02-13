{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterGetUserResponseParams@.
module ObjC.Matter.MTRDoorLockClusterGetUserResponseParams
  ( MTRDoorLockClusterGetUserResponseParams
  , IsMTRDoorLockClusterGetUserResponseParams(..)
  , initWithResponseValue_error
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
  , credentials
  , setCredentials
  , creatorFabricIndex
  , setCreatorFabricIndex
  , lastModifiedFabricIndex
  , setLastModifiedFabricIndex
  , nextUserIndex
  , setNextUserIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , userUniqueId
  , setUserUniqueId
  , creatorFabricIndexSelector
  , credentialRuleSelector
  , credentialsSelector
  , initWithResponseValue_errorSelector
  , lastModifiedFabricIndexSelector
  , nextUserIndexSelector
  , setCreatorFabricIndexSelector
  , setCredentialRuleSelector
  , setCredentialsSelector
  , setLastModifiedFabricIndexSelector
  , setNextUserIndexSelector
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

-- | Initialize an MTRDoorLockClusterGetUserResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterGetUserResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterGetUserResponseParams)
initWithResponseValue_error mtrDoorLockClusterGetUserResponseParams responseValue error_ =
  sendOwnedMessage mtrDoorLockClusterGetUserResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserIndexSelector (toNSNumber value)

-- | @- userName@
userName :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSString)
userName mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userNameSelector

-- | @- setUserName:@
setUserName :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSString value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserName mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserNameSelector (toNSString value)

-- | @- userUniqueID@
userUniqueID :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userUniqueID mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userUniqueIDSelector

-- | @- setUserUniqueID:@
setUserUniqueID :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserUniqueID mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserUniqueIDSelector (toNSNumber value)

-- | @- userStatus@
userStatus :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userStatus mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userStatusSelector

-- | @- setUserStatus:@
setUserStatus :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserStatus mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserStatusSelector (toNSNumber value)

-- | @- userType@
userType :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userType mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userTypeSelector

-- | @- setUserType:@
setUserType :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserType mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserTypeSelector (toNSNumber value)

-- | @- credentialRule@
credentialRule :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
credentialRule mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams credentialRuleSelector

-- | @- setCredentialRule:@
setCredentialRule :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCredentialRule mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setCredentialRuleSelector (toNSNumber value)

-- | @- credentials@
credentials :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSArray)
credentials mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSArray value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCredentials mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setCredentialsSelector (toNSArray value)

-- | @- creatorFabricIndex@
creatorFabricIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
creatorFabricIndex mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams creatorFabricIndexSelector

-- | @- setCreatorFabricIndex:@
setCreatorFabricIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setCreatorFabricIndex mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setCreatorFabricIndexSelector (toNSNumber value)

-- | @- lastModifiedFabricIndex@
lastModifiedFabricIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
lastModifiedFabricIndex mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams lastModifiedFabricIndexSelector

-- | @- setLastModifiedFabricIndex:@
setLastModifiedFabricIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setLastModifiedFabricIndex mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setLastModifiedFabricIndexSelector (toNSNumber value)

-- | @- nextUserIndex@
nextUserIndex :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
nextUserIndex mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams nextUserIndexSelector

-- | @- setNextUserIndex:@
setNextUserIndex :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setNextUserIndex mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setNextUserIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | @- userUniqueId@
userUniqueId :: IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams => mtrDoorLockClusterGetUserResponseParams -> IO (Id NSNumber)
userUniqueId mtrDoorLockClusterGetUserResponseParams =
  sendMessage mtrDoorLockClusterGetUserResponseParams userUniqueIdSelector

-- | @- setUserUniqueId:@
setUserUniqueId :: (IsMTRDoorLockClusterGetUserResponseParams mtrDoorLockClusterGetUserResponseParams, IsNSNumber value) => mtrDoorLockClusterGetUserResponseParams -> value -> IO ()
setUserUniqueId mtrDoorLockClusterGetUserResponseParams value =
  sendMessage mtrDoorLockClusterGetUserResponseParams setUserUniqueIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDoorLockClusterGetUserResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

-- | @Selector@ for @credentials@
credentialsSelector :: Selector '[] (Id NSArray)
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector '[Id NSArray] ()
setCredentialsSelector = mkSelector "setCredentials:"

-- | @Selector@ for @creatorFabricIndex@
creatorFabricIndexSelector :: Selector '[] (Id NSNumber)
creatorFabricIndexSelector = mkSelector "creatorFabricIndex"

-- | @Selector@ for @setCreatorFabricIndex:@
setCreatorFabricIndexSelector :: Selector '[Id NSNumber] ()
setCreatorFabricIndexSelector = mkSelector "setCreatorFabricIndex:"

-- | @Selector@ for @lastModifiedFabricIndex@
lastModifiedFabricIndexSelector :: Selector '[] (Id NSNumber)
lastModifiedFabricIndexSelector = mkSelector "lastModifiedFabricIndex"

-- | @Selector@ for @setLastModifiedFabricIndex:@
setLastModifiedFabricIndexSelector :: Selector '[Id NSNumber] ()
setLastModifiedFabricIndexSelector = mkSelector "setLastModifiedFabricIndex:"

-- | @Selector@ for @nextUserIndex@
nextUserIndexSelector :: Selector '[] (Id NSNumber)
nextUserIndexSelector = mkSelector "nextUserIndex"

-- | @Selector@ for @setNextUserIndex:@
setNextUserIndexSelector :: Selector '[Id NSNumber] ()
setNextUserIndexSelector = mkSelector "setNextUserIndex:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @userUniqueId@
userUniqueIdSelector :: Selector '[] (Id NSNumber)
userUniqueIdSelector = mkSelector "userUniqueId"

-- | @Selector@ for @setUserUniqueId:@
setUserUniqueIdSelector :: Selector '[Id NSNumber] ()
setUserUniqueIdSelector = mkSelector "setUserUniqueId:"


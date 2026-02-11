{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForAccountsIntentResponse@.
module ObjC.Intents.INSearchForAccountsIntentResponse
  ( INSearchForAccountsIntentResponse
  , IsINSearchForAccountsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , accounts
  , setAccounts
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , accountsSelector
  , setAccountsSelector

  -- * Enum types
  , INSearchForAccountsIntentResponseCode(INSearchForAccountsIntentResponseCode)
  , pattern INSearchForAccountsIntentResponseCodeUnspecified
  , pattern INSearchForAccountsIntentResponseCodeReady
  , pattern INSearchForAccountsIntentResponseCodeInProgress
  , pattern INSearchForAccountsIntentResponseCodeSuccess
  , pattern INSearchForAccountsIntentResponseCodeFailure
  , pattern INSearchForAccountsIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForAccountsIntentResponseCodeFailureCredentialsUnverified
  , pattern INSearchForAccountsIntentResponseCodeFailureAccountNotFound
  , pattern INSearchForAccountsIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired
  , pattern INSearchForAccountsIntentResponseCodeFailureNotEligible

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO RawId
init_ inSearchForAccountsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForAccountsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse, IsNSUserActivity userActivity) => inSearchForAccountsIntentResponse -> INSearchForAccountsIntentResponseCode -> userActivity -> IO (Id INSearchForAccountsIntentResponse)
initWithCode_userActivity inSearchForAccountsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForAccountsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO INSearchForAccountsIntentResponseCode
code inSearchForAccountsIntentResponse  =
  fmap (coerce :: CLong -> INSearchForAccountsIntentResponseCode) $ sendMsg inSearchForAccountsIntentResponse (mkSelector "code") retCLong []

-- | @- accounts@
accounts :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO (Id NSArray)
accounts inSearchForAccountsIntentResponse  =
  sendMsg inSearchForAccountsIntentResponse (mkSelector "accounts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccounts:@
setAccounts :: (IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse, IsNSArray value) => inSearchForAccountsIntentResponse -> value -> IO ()
setAccounts inSearchForAccountsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForAccountsIntentResponse (mkSelector "setAccounts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @accounts@
accountsSelector :: Selector
accountsSelector = mkSelector "accounts"

-- | @Selector@ for @setAccounts:@
setAccountsSelector :: Selector
setAccountsSelector = mkSelector "setAccounts:"


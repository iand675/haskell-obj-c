{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , accountsSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO RawId
init_ inSearchForAccountsIntentResponse =
  sendOwnedMessage inSearchForAccountsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse, IsNSUserActivity userActivity) => inSearchForAccountsIntentResponse -> INSearchForAccountsIntentResponseCode -> userActivity -> IO (Id INSearchForAccountsIntentResponse)
initWithCode_userActivity inSearchForAccountsIntentResponse code userActivity =
  sendOwnedMessage inSearchForAccountsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO INSearchForAccountsIntentResponseCode
code inSearchForAccountsIntentResponse =
  sendMessage inSearchForAccountsIntentResponse codeSelector

-- | @- accounts@
accounts :: IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse => inSearchForAccountsIntentResponse -> IO (Id NSArray)
accounts inSearchForAccountsIntentResponse =
  sendMessage inSearchForAccountsIntentResponse accountsSelector

-- | @- setAccounts:@
setAccounts :: (IsINSearchForAccountsIntentResponse inSearchForAccountsIntentResponse, IsNSArray value) => inSearchForAccountsIntentResponse -> value -> IO ()
setAccounts inSearchForAccountsIntentResponse value =
  sendMessage inSearchForAccountsIntentResponse setAccountsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForAccountsIntentResponseCode, Id NSUserActivity] (Id INSearchForAccountsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForAccountsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @accounts@
accountsSelector :: Selector '[] (Id NSArray)
accountsSelector = mkSelector "accounts"

-- | @Selector@ for @setAccounts:@
setAccountsSelector :: Selector '[Id NSArray] ()
setAccountsSelector = mkSelector "setAccounts:"


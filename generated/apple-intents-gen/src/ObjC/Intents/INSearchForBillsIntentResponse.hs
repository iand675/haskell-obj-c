{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForBillsIntentResponse@.
module ObjC.Intents.INSearchForBillsIntentResponse
  ( INSearchForBillsIntentResponse
  , IsINSearchForBillsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , bills
  , setBills
  , billsSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setBillsSelector

  -- * Enum types
  , INSearchForBillsIntentResponseCode(INSearchForBillsIntentResponseCode)
  , pattern INSearchForBillsIntentResponseCodeUnspecified
  , pattern INSearchForBillsIntentResponseCodeReady
  , pattern INSearchForBillsIntentResponseCodeInProgress
  , pattern INSearchForBillsIntentResponseCodeSuccess
  , pattern INSearchForBillsIntentResponseCodeFailure
  , pattern INSearchForBillsIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForBillsIntentResponseCodeFailureCredentialsUnverified
  , pattern INSearchForBillsIntentResponseCodeFailureBillNotFound

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
init_ :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO RawId
init_ inSearchForBillsIntentResponse =
  sendOwnedMessage inSearchForBillsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse, IsNSUserActivity userActivity) => inSearchForBillsIntentResponse -> INSearchForBillsIntentResponseCode -> userActivity -> IO (Id INSearchForBillsIntentResponse)
initWithCode_userActivity inSearchForBillsIntentResponse code userActivity =
  sendOwnedMessage inSearchForBillsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO INSearchForBillsIntentResponseCode
code inSearchForBillsIntentResponse =
  sendMessage inSearchForBillsIntentResponse codeSelector

-- | @- bills@
bills :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO (Id NSArray)
bills inSearchForBillsIntentResponse =
  sendMessage inSearchForBillsIntentResponse billsSelector

-- | @- setBills:@
setBills :: (IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse, IsNSArray value) => inSearchForBillsIntentResponse -> value -> IO ()
setBills inSearchForBillsIntentResponse value =
  sendMessage inSearchForBillsIntentResponse setBillsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForBillsIntentResponseCode, Id NSUserActivity] (Id INSearchForBillsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForBillsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @bills@
billsSelector :: Selector '[] (Id NSArray)
billsSelector = mkSelector "bills"

-- | @Selector@ for @setBills:@
setBillsSelector :: Selector '[Id NSArray] ()
setBillsSelector = mkSelector "setBills:"


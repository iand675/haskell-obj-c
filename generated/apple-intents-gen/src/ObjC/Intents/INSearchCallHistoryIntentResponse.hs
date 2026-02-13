{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchCallHistoryIntentResponse@.
module ObjC.Intents.INSearchCallHistoryIntentResponse
  ( INSearchCallHistoryIntentResponse
  , IsINSearchCallHistoryIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , callRecords
  , setCallRecords
  , callRecordsSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setCallRecordsSelector

  -- * Enum types
  , INSearchCallHistoryIntentResponseCode(INSearchCallHistoryIntentResponseCode)
  , pattern INSearchCallHistoryIntentResponseCodeUnspecified
  , pattern INSearchCallHistoryIntentResponseCodeReady
  , pattern INSearchCallHistoryIntentResponseCodeContinueInApp
  , pattern INSearchCallHistoryIntentResponseCodeFailure
  , pattern INSearchCallHistoryIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchCallHistoryIntentResponseCodeFailureAppConfigurationRequired
  , pattern INSearchCallHistoryIntentResponseCodeInProgress
  , pattern INSearchCallHistoryIntentResponseCodeSuccess

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
init_ :: IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse => inSearchCallHistoryIntentResponse -> IO RawId
init_ inSearchCallHistoryIntentResponse =
  sendOwnedMessage inSearchCallHistoryIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse, IsNSUserActivity userActivity) => inSearchCallHistoryIntentResponse -> INSearchCallHistoryIntentResponseCode -> userActivity -> IO (Id INSearchCallHistoryIntentResponse)
initWithCode_userActivity inSearchCallHistoryIntentResponse code userActivity =
  sendOwnedMessage inSearchCallHistoryIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse => inSearchCallHistoryIntentResponse -> IO INSearchCallHistoryIntentResponseCode
code inSearchCallHistoryIntentResponse =
  sendMessage inSearchCallHistoryIntentResponse codeSelector

-- | @- callRecords@
callRecords :: IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse => inSearchCallHistoryIntentResponse -> IO (Id NSArray)
callRecords inSearchCallHistoryIntentResponse =
  sendMessage inSearchCallHistoryIntentResponse callRecordsSelector

-- | @- setCallRecords:@
setCallRecords :: (IsINSearchCallHistoryIntentResponse inSearchCallHistoryIntentResponse, IsNSArray value) => inSearchCallHistoryIntentResponse -> value -> IO ()
setCallRecords inSearchCallHistoryIntentResponse value =
  sendMessage inSearchCallHistoryIntentResponse setCallRecordsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchCallHistoryIntentResponseCode, Id NSUserActivity] (Id INSearchCallHistoryIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchCallHistoryIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @callRecords@
callRecordsSelector :: Selector '[] (Id NSArray)
callRecordsSelector = mkSelector "callRecords"

-- | @Selector@ for @setCallRecords:@
setCallRecordsSelector :: Selector '[Id NSArray] ()
setCallRecordsSelector = mkSelector "setCallRecords:"


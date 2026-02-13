{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAnswerCallIntentResponse@.
module ObjC.Intents.INAnswerCallIntentResponse
  ( INAnswerCallIntentResponse
  , IsINAnswerCallIntentResponse(..)
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
  , INAnswerCallIntentResponseCode(INAnswerCallIntentResponseCode)
  , pattern INAnswerCallIntentResponseCodeUnspecified
  , pattern INAnswerCallIntentResponseCodeReady
  , pattern INAnswerCallIntentResponseCodeContinueInApp
  , pattern INAnswerCallIntentResponseCodeInProgress
  , pattern INAnswerCallIntentResponseCodeSuccess
  , pattern INAnswerCallIntentResponseCodeFailure
  , pattern INAnswerCallIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO RawId
init_ inAnswerCallIntentResponse =
  sendOwnedMessage inAnswerCallIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAnswerCallIntentResponse inAnswerCallIntentResponse, IsNSUserActivity userActivity) => inAnswerCallIntentResponse -> INAnswerCallIntentResponseCode -> userActivity -> IO (Id INAnswerCallIntentResponse)
initWithCode_userActivity inAnswerCallIntentResponse code userActivity =
  sendOwnedMessage inAnswerCallIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO INAnswerCallIntentResponseCode
code inAnswerCallIntentResponse =
  sendMessage inAnswerCallIntentResponse codeSelector

-- | @- callRecords@
callRecords :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO (Id NSArray)
callRecords inAnswerCallIntentResponse =
  sendMessage inAnswerCallIntentResponse callRecordsSelector

-- | @- setCallRecords:@
setCallRecords :: (IsINAnswerCallIntentResponse inAnswerCallIntentResponse, IsNSArray value) => inAnswerCallIntentResponse -> value -> IO ()
setCallRecords inAnswerCallIntentResponse value =
  sendMessage inAnswerCallIntentResponse setCallRecordsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INAnswerCallIntentResponseCode, Id NSUserActivity] (Id INAnswerCallIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INAnswerCallIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @callRecords@
callRecordsSelector :: Selector '[] (Id NSArray)
callRecordsSelector = mkSelector "callRecords"

-- | @Selector@ for @setCallRecords:@
setCallRecordsSelector :: Selector '[Id NSArray] ()
setCallRecordsSelector = mkSelector "setCallRecords:"


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEditMessageIntentResponse@.
module ObjC.Intents.INEditMessageIntentResponse
  ( INEditMessageIntentResponse
  , IsINEditMessageIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INEditMessageIntentResponseCode(INEditMessageIntentResponseCode)
  , pattern INEditMessageIntentResponseCodeUnspecified
  , pattern INEditMessageIntentResponseCodeReady
  , pattern INEditMessageIntentResponseCodeInProgress
  , pattern INEditMessageIntentResponseCodeSuccess
  , pattern INEditMessageIntentResponseCodeFailure
  , pattern INEditMessageIntentResponseCodeFailureRequiringAppLaunch
  , pattern INEditMessageIntentResponseCodeFailureMessageNotFound
  , pattern INEditMessageIntentResponseCodeFailurePastEditTimeLimit
  , pattern INEditMessageIntentResponseCodeFailureMessageTypeUnsupported
  , pattern INEditMessageIntentResponseCodeFailureUnsupportedOnService
  , pattern INEditMessageIntentResponseCodeFailureMessageServiceNotAvailable
  , pattern INEditMessageIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINEditMessageIntentResponse inEditMessageIntentResponse => inEditMessageIntentResponse -> IO RawId
init_ inEditMessageIntentResponse =
  sendOwnedMessage inEditMessageIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINEditMessageIntentResponse inEditMessageIntentResponse, IsNSUserActivity userActivity) => inEditMessageIntentResponse -> INEditMessageIntentResponseCode -> userActivity -> IO (Id INEditMessageIntentResponse)
initWithCode_userActivity inEditMessageIntentResponse code userActivity =
  sendOwnedMessage inEditMessageIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINEditMessageIntentResponse inEditMessageIntentResponse => inEditMessageIntentResponse -> IO INEditMessageIntentResponseCode
code inEditMessageIntentResponse =
  sendMessage inEditMessageIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INEditMessageIntentResponseCode, Id NSUserActivity] (Id INEditMessageIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INEditMessageIntentResponseCode
codeSelector = mkSelector "code"


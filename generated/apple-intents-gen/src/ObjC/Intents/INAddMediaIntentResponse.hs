{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaIntentResponse@.
module ObjC.Intents.INAddMediaIntentResponse
  ( INAddMediaIntentResponse
  , IsINAddMediaIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INAddMediaIntentResponseCode(INAddMediaIntentResponseCode)
  , pattern INAddMediaIntentResponseCodeUnspecified
  , pattern INAddMediaIntentResponseCodeReady
  , pattern INAddMediaIntentResponseCodeInProgress
  , pattern INAddMediaIntentResponseCodeSuccess
  , pattern INAddMediaIntentResponseCodeHandleInApp
  , pattern INAddMediaIntentResponseCodeFailure
  , pattern INAddMediaIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAddMediaIntentResponse inAddMediaIntentResponse => inAddMediaIntentResponse -> IO RawId
init_ inAddMediaIntentResponse =
  sendOwnedMessage inAddMediaIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAddMediaIntentResponse inAddMediaIntentResponse, IsNSUserActivity userActivity) => inAddMediaIntentResponse -> INAddMediaIntentResponseCode -> userActivity -> IO (Id INAddMediaIntentResponse)
initWithCode_userActivity inAddMediaIntentResponse code userActivity =
  sendOwnedMessage inAddMediaIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINAddMediaIntentResponse inAddMediaIntentResponse => inAddMediaIntentResponse -> IO INAddMediaIntentResponseCode
code inAddMediaIntentResponse =
  sendMessage inAddMediaIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INAddMediaIntentResponseCode, Id NSUserActivity] (Id INAddMediaIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INAddMediaIntentResponseCode
codeSelector = mkSelector "code"


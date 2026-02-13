{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpdateMediaAffinityIntentResponse@.
module ObjC.Intents.INUpdateMediaAffinityIntentResponse
  ( INUpdateMediaAffinityIntentResponse
  , IsINUpdateMediaAffinityIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INUpdateMediaAffinityIntentResponseCode(INUpdateMediaAffinityIntentResponseCode)
  , pattern INUpdateMediaAffinityIntentResponseCodeUnspecified
  , pattern INUpdateMediaAffinityIntentResponseCodeReady
  , pattern INUpdateMediaAffinityIntentResponseCodeInProgress
  , pattern INUpdateMediaAffinityIntentResponseCodeSuccess
  , pattern INUpdateMediaAffinityIntentResponseCodeFailure
  , pattern INUpdateMediaAffinityIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse => inUpdateMediaAffinityIntentResponse -> IO RawId
init_ inUpdateMediaAffinityIntentResponse =
  sendOwnedMessage inUpdateMediaAffinityIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse, IsNSUserActivity userActivity) => inUpdateMediaAffinityIntentResponse -> INUpdateMediaAffinityIntentResponseCode -> userActivity -> IO (Id INUpdateMediaAffinityIntentResponse)
initWithCode_userActivity inUpdateMediaAffinityIntentResponse code userActivity =
  sendOwnedMessage inUpdateMediaAffinityIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINUpdateMediaAffinityIntentResponse inUpdateMediaAffinityIntentResponse => inUpdateMediaAffinityIntentResponse -> IO INUpdateMediaAffinityIntentResponseCode
code inUpdateMediaAffinityIntentResponse =
  sendMessage inUpdateMediaAffinityIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INUpdateMediaAffinityIntentResponseCode, Id NSUserActivity] (Id INUpdateMediaAffinityIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INUpdateMediaAffinityIntentResponseCode
codeSelector = mkSelector "code"


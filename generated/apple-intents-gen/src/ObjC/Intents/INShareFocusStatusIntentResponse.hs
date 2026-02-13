{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INShareFocusStatusIntentResponse@.
module ObjC.Intents.INShareFocusStatusIntentResponse
  ( INShareFocusStatusIntentResponse
  , IsINShareFocusStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INShareFocusStatusIntentResponseCode(INShareFocusStatusIntentResponseCode)
  , pattern INShareFocusStatusIntentResponseCodeUnspecified
  , pattern INShareFocusStatusIntentResponseCodeReady
  , pattern INShareFocusStatusIntentResponseCodeInProgress
  , pattern INShareFocusStatusIntentResponseCodeSuccess
  , pattern INShareFocusStatusIntentResponseCodeFailure
  , pattern INShareFocusStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse => inShareFocusStatusIntentResponse -> IO RawId
init_ inShareFocusStatusIntentResponse =
  sendOwnedMessage inShareFocusStatusIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse, IsNSUserActivity userActivity) => inShareFocusStatusIntentResponse -> INShareFocusStatusIntentResponseCode -> userActivity -> IO (Id INShareFocusStatusIntentResponse)
initWithCode_userActivity inShareFocusStatusIntentResponse code userActivity =
  sendOwnedMessage inShareFocusStatusIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse => inShareFocusStatusIntentResponse -> IO INShareFocusStatusIntentResponseCode
code inShareFocusStatusIntentResponse =
  sendMessage inShareFocusStatusIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INShareFocusStatusIntentResponseCode, Id NSUserActivity] (Id INShareFocusStatusIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INShareFocusStatusIntentResponseCode
codeSelector = mkSelector "code"


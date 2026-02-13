{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetCarLockStatusIntentResponse@.
module ObjC.Intents.INSetCarLockStatusIntentResponse
  ( INSetCarLockStatusIntentResponse
  , IsINSetCarLockStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetCarLockStatusIntentResponseCode(INSetCarLockStatusIntentResponseCode)
  , pattern INSetCarLockStatusIntentResponseCodeUnspecified
  , pattern INSetCarLockStatusIntentResponseCodeReady
  , pattern INSetCarLockStatusIntentResponseCodeInProgress
  , pattern INSetCarLockStatusIntentResponseCodeSuccess
  , pattern INSetCarLockStatusIntentResponseCodeFailure
  , pattern INSetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse => inSetCarLockStatusIntentResponse -> IO RawId
init_ inSetCarLockStatusIntentResponse =
  sendOwnedMessage inSetCarLockStatusIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse, IsNSUserActivity userActivity) => inSetCarLockStatusIntentResponse -> INSetCarLockStatusIntentResponseCode -> userActivity -> IO (Id INSetCarLockStatusIntentResponse)
initWithCode_userActivity inSetCarLockStatusIntentResponse code userActivity =
  sendOwnedMessage inSetCarLockStatusIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse => inSetCarLockStatusIntentResponse -> IO INSetCarLockStatusIntentResponseCode
code inSetCarLockStatusIntentResponse =
  sendMessage inSetCarLockStatusIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetCarLockStatusIntentResponseCode, Id NSUserActivity] (Id INSetCarLockStatusIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetCarLockStatusIntentResponseCode
codeSelector = mkSelector "code"


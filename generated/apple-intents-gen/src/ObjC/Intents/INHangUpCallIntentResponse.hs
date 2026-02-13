{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INHangUpCallIntentResponse@.
module ObjC.Intents.INHangUpCallIntentResponse
  ( INHangUpCallIntentResponse
  , IsINHangUpCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INHangUpCallIntentResponseCode(INHangUpCallIntentResponseCode)
  , pattern INHangUpCallIntentResponseCodeUnspecified
  , pattern INHangUpCallIntentResponseCodeReady
  , pattern INHangUpCallIntentResponseCodeInProgress
  , pattern INHangUpCallIntentResponseCodeSuccess
  , pattern INHangUpCallIntentResponseCodeFailure
  , pattern INHangUpCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INHangUpCallIntentResponseCodeFailureNoCallToHangUp

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
init_ :: IsINHangUpCallIntentResponse inHangUpCallIntentResponse => inHangUpCallIntentResponse -> IO RawId
init_ inHangUpCallIntentResponse =
  sendOwnedMessage inHangUpCallIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINHangUpCallIntentResponse inHangUpCallIntentResponse, IsNSUserActivity userActivity) => inHangUpCallIntentResponse -> INHangUpCallIntentResponseCode -> userActivity -> IO (Id INHangUpCallIntentResponse)
initWithCode_userActivity inHangUpCallIntentResponse code userActivity =
  sendOwnedMessage inHangUpCallIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINHangUpCallIntentResponse inHangUpCallIntentResponse => inHangUpCallIntentResponse -> IO INHangUpCallIntentResponseCode
code inHangUpCallIntentResponse =
  sendMessage inHangUpCallIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INHangUpCallIntentResponseCode, Id NSUserActivity] (Id INHangUpCallIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INHangUpCallIntentResponseCode
codeSelector = mkSelector "code"


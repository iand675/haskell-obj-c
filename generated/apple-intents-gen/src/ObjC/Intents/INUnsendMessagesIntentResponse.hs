{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUnsendMessagesIntentResponse@.
module ObjC.Intents.INUnsendMessagesIntentResponse
  ( INUnsendMessagesIntentResponse
  , IsINUnsendMessagesIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INUnsendMessagesIntentResponseCode(INUnsendMessagesIntentResponseCode)
  , pattern INUnsendMessagesIntentResponseCodeUnspecified
  , pattern INUnsendMessagesIntentResponseCodeReady
  , pattern INUnsendMessagesIntentResponseCodeInProgress
  , pattern INUnsendMessagesIntentResponseCodeSuccess
  , pattern INUnsendMessagesIntentResponseCodeFailure
  , pattern INUnsendMessagesIntentResponseCodeFailureRequiringAppLaunch
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageNotFound
  , pattern INUnsendMessagesIntentResponseCodeFailurePastUnsendTimeLimit
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageTypeUnsupported
  , pattern INUnsendMessagesIntentResponseCodeFailureUnsupportedOnService
  , pattern INUnsendMessagesIntentResponseCodeFailureMessageServiceNotAvailable
  , pattern INUnsendMessagesIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse => inUnsendMessagesIntentResponse -> IO RawId
init_ inUnsendMessagesIntentResponse =
  sendOwnedMessage inUnsendMessagesIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse, IsNSUserActivity userActivity) => inUnsendMessagesIntentResponse -> INUnsendMessagesIntentResponseCode -> userActivity -> IO (Id INUnsendMessagesIntentResponse)
initWithCode_userActivity inUnsendMessagesIntentResponse code userActivity =
  sendOwnedMessage inUnsendMessagesIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINUnsendMessagesIntentResponse inUnsendMessagesIntentResponse => inUnsendMessagesIntentResponse -> IO INUnsendMessagesIntentResponseCode
code inUnsendMessagesIntentResponse =
  sendMessage inUnsendMessagesIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INUnsendMessagesIntentResponseCode, Id NSUserActivity] (Id INUnsendMessagesIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INUnsendMessagesIntentResponseCode
codeSelector = mkSelector "code"


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendRideFeedbackIntentResponse@.
module ObjC.Intents.INSendRideFeedbackIntentResponse
  ( INSendRideFeedbackIntentResponse
  , IsINSendRideFeedbackIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSendRideFeedbackIntentResponseCode(INSendRideFeedbackIntentResponseCode)
  , pattern INSendRideFeedbackIntentResponseCodeUnspecified
  , pattern INSendRideFeedbackIntentResponseCodeReady
  , pattern INSendRideFeedbackIntentResponseCodeSuccess
  , pattern INSendRideFeedbackIntentResponseCodeFailure

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
init_ :: IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse => inSendRideFeedbackIntentResponse -> IO RawId
init_ inSendRideFeedbackIntentResponse =
  sendOwnedMessage inSendRideFeedbackIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse, IsNSUserActivity userActivity) => inSendRideFeedbackIntentResponse -> INSendRideFeedbackIntentResponseCode -> userActivity -> IO (Id INSendRideFeedbackIntentResponse)
initWithCode_userActivity inSendRideFeedbackIntentResponse code userActivity =
  sendOwnedMessage inSendRideFeedbackIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse => inSendRideFeedbackIntentResponse -> IO INSendRideFeedbackIntentResponseCode
code inSendRideFeedbackIntentResponse =
  sendMessage inSendRideFeedbackIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSendRideFeedbackIntentResponseCode, Id NSUserActivity] (Id INSendRideFeedbackIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSendRideFeedbackIntentResponseCode
codeSelector = mkSelector "code"


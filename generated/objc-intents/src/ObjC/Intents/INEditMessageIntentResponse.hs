{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINEditMessageIntentResponse inEditMessageIntentResponse => inEditMessageIntentResponse -> IO RawId
init_ inEditMessageIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inEditMessageIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINEditMessageIntentResponse inEditMessageIntentResponse, IsNSUserActivity userActivity) => inEditMessageIntentResponse -> INEditMessageIntentResponseCode -> userActivity -> IO (Id INEditMessageIntentResponse)
initWithCode_userActivity inEditMessageIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inEditMessageIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINEditMessageIntentResponse inEditMessageIntentResponse => inEditMessageIntentResponse -> IO INEditMessageIntentResponseCode
code inEditMessageIntentResponse  =
  fmap (coerce :: CLong -> INEditMessageIntentResponseCode) $ sendMsg inEditMessageIntentResponse (mkSelector "code") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"


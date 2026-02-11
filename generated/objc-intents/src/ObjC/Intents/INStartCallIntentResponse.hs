{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallIntentResponse@.
module ObjC.Intents.INStartCallIntentResponse
  ( INStartCallIntentResponse
  , IsINStartCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INStartCallIntentResponseCode(INStartCallIntentResponseCode)
  , pattern INStartCallIntentResponseCodeUnspecified
  , pattern INStartCallIntentResponseCodeReady
  , pattern INStartCallIntentResponseCodeContinueInApp
  , pattern INStartCallIntentResponseCodeUserConfirmationRequired
  , pattern INStartCallIntentResponseCodeFailure
  , pattern INStartCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartCallIntentResponseCodeFailureCallingServiceNotAvailable
  , pattern INStartCallIntentResponseCodeFailureContactNotSupportedByApp
  , pattern INStartCallIntentResponseCodeFailureAirplaneModeEnabled
  , pattern INStartCallIntentResponseCodeFailureUnableToHandOff
  , pattern INStartCallIntentResponseCodeFailureAppConfigurationRequired
  , pattern INStartCallIntentResponseCodeFailureCallInProgress
  , pattern INStartCallIntentResponseCodeFailureCallRinging
  , pattern INStartCallIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINStartCallIntentResponse inStartCallIntentResponse => inStartCallIntentResponse -> IO RawId
init_ inStartCallIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inStartCallIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartCallIntentResponse inStartCallIntentResponse, IsNSUserActivity userActivity) => inStartCallIntentResponse -> INStartCallIntentResponseCode -> userActivity -> IO (Id INStartCallIntentResponse)
initWithCode_userActivity inStartCallIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inStartCallIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINStartCallIntentResponse inStartCallIntentResponse => inStartCallIntentResponse -> IO INStartCallIntentResponseCode
code inStartCallIntentResponse  =
  fmap (coerce :: CLong -> INStartCallIntentResponseCode) $ sendMsg inStartCallIntentResponse (mkSelector "code") retCLong []

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


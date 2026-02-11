{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartVideoCallIntentResponse@.
module ObjC.Intents.INStartVideoCallIntentResponse
  ( INStartVideoCallIntentResponse
  , IsINStartVideoCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INStartVideoCallIntentResponseCode(INStartVideoCallIntentResponseCode)
  , pattern INStartVideoCallIntentResponseCodeUnspecified
  , pattern INStartVideoCallIntentResponseCodeReady
  , pattern INStartVideoCallIntentResponseCodeContinueInApp
  , pattern INStartVideoCallIntentResponseCodeFailure
  , pattern INStartVideoCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartVideoCallIntentResponseCodeFailureAppConfigurationRequired
  , pattern INStartVideoCallIntentResponseCodeFailureCallingServiceNotAvailable
  , pattern INStartVideoCallIntentResponseCodeFailureContactNotSupportedByApp
  , pattern INStartVideoCallIntentResponseCodeFailureInvalidNumber

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
init_ :: IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse => inStartVideoCallIntentResponse -> IO RawId
init_ inStartVideoCallIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inStartVideoCallIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse, IsNSUserActivity userActivity) => inStartVideoCallIntentResponse -> INStartVideoCallIntentResponseCode -> userActivity -> IO (Id INStartVideoCallIntentResponse)
initWithCode_userActivity inStartVideoCallIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inStartVideoCallIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse => inStartVideoCallIntentResponse -> IO INStartVideoCallIntentResponseCode
code inStartVideoCallIntentResponse  =
  fmap (coerce :: CLong -> INStartVideoCallIntentResponseCode) $ sendMsg inStartVideoCallIntentResponse (mkSelector "code") retCLong []

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


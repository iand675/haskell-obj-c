{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartPhotoPlaybackIntentResponse@.
module ObjC.Intents.INStartPhotoPlaybackIntentResponse
  ( INStartPhotoPlaybackIntentResponse
  , IsINStartPhotoPlaybackIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INStartPhotoPlaybackIntentResponseCode(INStartPhotoPlaybackIntentResponseCode)
  , pattern INStartPhotoPlaybackIntentResponseCodeUnspecified
  , pattern INStartPhotoPlaybackIntentResponseCodeReady
  , pattern INStartPhotoPlaybackIntentResponseCodeContinueInApp
  , pattern INStartPhotoPlaybackIntentResponseCodeFailure
  , pattern INStartPhotoPlaybackIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartPhotoPlaybackIntentResponseCodeFailureAppConfigurationRequired

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
init_ :: IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse => inStartPhotoPlaybackIntentResponse -> IO RawId
init_ inStartPhotoPlaybackIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inStartPhotoPlaybackIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse, IsNSUserActivity userActivity) => inStartPhotoPlaybackIntentResponse -> INStartPhotoPlaybackIntentResponseCode -> userActivity -> IO (Id INStartPhotoPlaybackIntentResponse)
initWithCode_userActivity inStartPhotoPlaybackIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inStartPhotoPlaybackIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINStartPhotoPlaybackIntentResponse inStartPhotoPlaybackIntentResponse => inStartPhotoPlaybackIntentResponse -> IO INStartPhotoPlaybackIntentResponseCode
code inStartPhotoPlaybackIntentResponse  =
  fmap (coerce :: CLong -> INStartPhotoPlaybackIntentResponseCode) $ sendMsg inStartPhotoPlaybackIntentResponse (mkSelector "code") retCLong []

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


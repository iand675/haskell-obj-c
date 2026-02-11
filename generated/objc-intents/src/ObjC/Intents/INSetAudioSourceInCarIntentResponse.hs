{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetAudioSourceInCarIntentResponse@.
module ObjC.Intents.INSetAudioSourceInCarIntentResponse
  ( INSetAudioSourceInCarIntentResponse
  , IsINSetAudioSourceInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetAudioSourceInCarIntentResponseCode(INSetAudioSourceInCarIntentResponseCode)
  , pattern INSetAudioSourceInCarIntentResponseCodeUnspecified
  , pattern INSetAudioSourceInCarIntentResponseCodeReady
  , pattern INSetAudioSourceInCarIntentResponseCodeInProgress
  , pattern INSetAudioSourceInCarIntentResponseCodeSuccess
  , pattern INSetAudioSourceInCarIntentResponseCodeFailure
  , pattern INSetAudioSourceInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse => inSetAudioSourceInCarIntentResponse -> IO RawId
init_ inSetAudioSourceInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetAudioSourceInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse, IsNSUserActivity userActivity) => inSetAudioSourceInCarIntentResponse -> INSetAudioSourceInCarIntentResponseCode -> userActivity -> IO (Id INSetAudioSourceInCarIntentResponse)
initWithCode_userActivity inSetAudioSourceInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetAudioSourceInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse => inSetAudioSourceInCarIntentResponse -> IO INSetAudioSourceInCarIntentResponseCode
code inSetAudioSourceInCarIntentResponse  =
  fmap (coerce :: CLong -> INSetAudioSourceInCarIntentResponseCode) $ sendMsg inSetAudioSourceInCarIntentResponse (mkSelector "code") retCLong []

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


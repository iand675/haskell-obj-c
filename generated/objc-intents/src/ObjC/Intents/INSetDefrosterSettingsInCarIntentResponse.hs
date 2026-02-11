{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetDefrosterSettingsInCarIntentResponse@.
module ObjC.Intents.INSetDefrosterSettingsInCarIntentResponse
  ( INSetDefrosterSettingsInCarIntentResponse
  , IsINSetDefrosterSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetDefrosterSettingsInCarIntentResponseCode(INSetDefrosterSettingsInCarIntentResponseCode)
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeReady
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeInProgress
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeSuccess
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeFailure
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse => inSetDefrosterSettingsInCarIntentResponse -> IO RawId
init_ inSetDefrosterSettingsInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetDefrosterSettingsInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetDefrosterSettingsInCarIntentResponse -> INSetDefrosterSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetDefrosterSettingsInCarIntentResponse)
initWithCode_userActivity inSetDefrosterSettingsInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetDefrosterSettingsInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse => inSetDefrosterSettingsInCarIntentResponse -> IO INSetDefrosterSettingsInCarIntentResponseCode
code inSetDefrosterSettingsInCarIntentResponse  =
  fmap (coerce :: CLong -> INSetDefrosterSettingsInCarIntentResponseCode) $ sendMsg inSetDefrosterSettingsInCarIntentResponse (mkSelector "code") retCLong []

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


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetSeatSettingsInCarIntentResponse@.
module ObjC.Intents.INSetSeatSettingsInCarIntentResponse
  ( INSetSeatSettingsInCarIntentResponse
  , IsINSetSeatSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetSeatSettingsInCarIntentResponseCode(INSetSeatSettingsInCarIntentResponseCode)
  , pattern INSetSeatSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetSeatSettingsInCarIntentResponseCodeReady
  , pattern INSetSeatSettingsInCarIntentResponseCodeInProgress
  , pattern INSetSeatSettingsInCarIntentResponseCodeSuccess
  , pattern INSetSeatSettingsInCarIntentResponseCodeFailure
  , pattern INSetSeatSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse => inSetSeatSettingsInCarIntentResponse -> IO RawId
init_ inSetSeatSettingsInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetSeatSettingsInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetSeatSettingsInCarIntentResponse -> INSetSeatSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetSeatSettingsInCarIntentResponse)
initWithCode_userActivity inSetSeatSettingsInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetSeatSettingsInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse => inSetSeatSettingsInCarIntentResponse -> IO INSetSeatSettingsInCarIntentResponseCode
code inSetSeatSettingsInCarIntentResponse  =
  fmap (coerce :: CLong -> INSetSeatSettingsInCarIntentResponseCode) $ sendMsg inSetSeatSettingsInCarIntentResponse (mkSelector "code") retCLong []

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


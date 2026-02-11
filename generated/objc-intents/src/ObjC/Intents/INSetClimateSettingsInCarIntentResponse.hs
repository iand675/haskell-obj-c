{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetClimateSettingsInCarIntentResponse@.
module ObjC.Intents.INSetClimateSettingsInCarIntentResponse
  ( INSetClimateSettingsInCarIntentResponse
  , IsINSetClimateSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetClimateSettingsInCarIntentResponseCode(INSetClimateSettingsInCarIntentResponseCode)
  , pattern INSetClimateSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetClimateSettingsInCarIntentResponseCodeReady
  , pattern INSetClimateSettingsInCarIntentResponseCodeInProgress
  , pattern INSetClimateSettingsInCarIntentResponseCodeSuccess
  , pattern INSetClimateSettingsInCarIntentResponseCodeFailure
  , pattern INSetClimateSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse => inSetClimateSettingsInCarIntentResponse -> IO RawId
init_ inSetClimateSettingsInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetClimateSettingsInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetClimateSettingsInCarIntentResponse -> INSetClimateSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetClimateSettingsInCarIntentResponse)
initWithCode_userActivity inSetClimateSettingsInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetClimateSettingsInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse => inSetClimateSettingsInCarIntentResponse -> IO INSetClimateSettingsInCarIntentResponseCode
code inSetClimateSettingsInCarIntentResponse  =
  fmap (coerce :: CLong -> INSetClimateSettingsInCarIntentResponseCode) $ sendMsg inSetClimateSettingsInCarIntentResponse (mkSelector "code") retCLong []

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


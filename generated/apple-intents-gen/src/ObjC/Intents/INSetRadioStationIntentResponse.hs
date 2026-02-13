{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetRadioStationIntentResponse@.
module ObjC.Intents.INSetRadioStationIntentResponse
  ( INSetRadioStationIntentResponse
  , IsINSetRadioStationIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetRadioStationIntentResponseCode(INSetRadioStationIntentResponseCode)
  , pattern INSetRadioStationIntentResponseCodeUnspecified
  , pattern INSetRadioStationIntentResponseCodeReady
  , pattern INSetRadioStationIntentResponseCodeInProgress
  , pattern INSetRadioStationIntentResponseCodeSuccess
  , pattern INSetRadioStationIntentResponseCodeFailure
  , pattern INSetRadioStationIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSetRadioStationIntentResponseCodeFailureNotSubscribed

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
init_ :: IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse => inSetRadioStationIntentResponse -> IO RawId
init_ inSetRadioStationIntentResponse =
  sendOwnedMessage inSetRadioStationIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse, IsNSUserActivity userActivity) => inSetRadioStationIntentResponse -> INSetRadioStationIntentResponseCode -> userActivity -> IO (Id INSetRadioStationIntentResponse)
initWithCode_userActivity inSetRadioStationIntentResponse code userActivity =
  sendOwnedMessage inSetRadioStationIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse => inSetRadioStationIntentResponse -> IO INSetRadioStationIntentResponseCode
code inSetRadioStationIntentResponse =
  sendMessage inSetRadioStationIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetRadioStationIntentResponseCode, Id NSUserActivity] (Id INSetRadioStationIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetRadioStationIntentResponseCode
codeSelector = mkSelector "code"


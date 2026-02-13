{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INActivateCarSignalIntentResponse@.
module ObjC.Intents.INActivateCarSignalIntentResponse
  ( INActivateCarSignalIntentResponse
  , IsINActivateCarSignalIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , signals
  , setSignals
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setSignalsSelector
  , signalsSelector

  -- * Enum types
  , INActivateCarSignalIntentResponseCode(INActivateCarSignalIntentResponseCode)
  , pattern INActivateCarSignalIntentResponseCodeUnspecified
  , pattern INActivateCarSignalIntentResponseCodeReady
  , pattern INActivateCarSignalIntentResponseCodeInProgress
  , pattern INActivateCarSignalIntentResponseCodeSuccess
  , pattern INActivateCarSignalIntentResponseCodeFailure
  , pattern INActivateCarSignalIntentResponseCodeFailureRequiringAppLaunch
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

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
init_ :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO RawId
init_ inActivateCarSignalIntentResponse =
  sendOwnedMessage inActivateCarSignalIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse, IsNSUserActivity userActivity) => inActivateCarSignalIntentResponse -> INActivateCarSignalIntentResponseCode -> userActivity -> IO (Id INActivateCarSignalIntentResponse)
initWithCode_userActivity inActivateCarSignalIntentResponse code userActivity =
  sendOwnedMessage inActivateCarSignalIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO INActivateCarSignalIntentResponseCode
code inActivateCarSignalIntentResponse =
  sendMessage inActivateCarSignalIntentResponse codeSelector

-- | @- signals@
signals :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO INCarSignalOptions
signals inActivateCarSignalIntentResponse =
  sendMessage inActivateCarSignalIntentResponse signalsSelector

-- | @- setSignals:@
setSignals :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> INCarSignalOptions -> IO ()
setSignals inActivateCarSignalIntentResponse value =
  sendMessage inActivateCarSignalIntentResponse setSignalsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INActivateCarSignalIntentResponseCode, Id NSUserActivity] (Id INActivateCarSignalIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INActivateCarSignalIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @signals@
signalsSelector :: Selector '[] INCarSignalOptions
signalsSelector = mkSelector "signals"

-- | @Selector@ for @setSignals:@
setSignalsSelector :: Selector '[INCarSignalOptions] ()
setSignalsSelector = mkSelector "setSignals:"


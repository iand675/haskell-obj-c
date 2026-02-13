{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetVisualCodeIntentResponse@.
module ObjC.Intents.INGetVisualCodeIntentResponse
  ( INGetVisualCodeIntentResponse
  , IsINGetVisualCodeIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , visualCodeImage
  , setVisualCodeImage
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setVisualCodeImageSelector
  , visualCodeImageSelector

  -- * Enum types
  , INGetVisualCodeIntentResponseCode(INGetVisualCodeIntentResponseCode)
  , pattern INGetVisualCodeIntentResponseCodeUnspecified
  , pattern INGetVisualCodeIntentResponseCodeReady
  , pattern INGetVisualCodeIntentResponseCodeContinueInApp
  , pattern INGetVisualCodeIntentResponseCodeInProgress
  , pattern INGetVisualCodeIntentResponseCodeSuccess
  , pattern INGetVisualCodeIntentResponseCodeFailure
  , pattern INGetVisualCodeIntentResponseCodeFailureRequiringAppLaunch
  , pattern INGetVisualCodeIntentResponseCodeFailureAppConfigurationRequired

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
init_ :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO RawId
init_ inGetVisualCodeIntentResponse =
  sendOwnedMessage inGetVisualCodeIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse, IsNSUserActivity userActivity) => inGetVisualCodeIntentResponse -> INGetVisualCodeIntentResponseCode -> userActivity -> IO (Id INGetVisualCodeIntentResponse)
initWithCode_userActivity inGetVisualCodeIntentResponse code userActivity =
  sendOwnedMessage inGetVisualCodeIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO INGetVisualCodeIntentResponseCode
code inGetVisualCodeIntentResponse =
  sendMessage inGetVisualCodeIntentResponse codeSelector

-- | @- visualCodeImage@
visualCodeImage :: IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse => inGetVisualCodeIntentResponse -> IO (Id INImage)
visualCodeImage inGetVisualCodeIntentResponse =
  sendMessage inGetVisualCodeIntentResponse visualCodeImageSelector

-- | @- setVisualCodeImage:@
setVisualCodeImage :: (IsINGetVisualCodeIntentResponse inGetVisualCodeIntentResponse, IsINImage value) => inGetVisualCodeIntentResponse -> value -> IO ()
setVisualCodeImage inGetVisualCodeIntentResponse value =
  sendMessage inGetVisualCodeIntentResponse setVisualCodeImageSelector (toINImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetVisualCodeIntentResponseCode, Id NSUserActivity] (Id INGetVisualCodeIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetVisualCodeIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @visualCodeImage@
visualCodeImageSelector :: Selector '[] (Id INImage)
visualCodeImageSelector = mkSelector "visualCodeImage"

-- | @Selector@ for @setVisualCodeImage:@
setVisualCodeImageSelector :: Selector '[Id INImage] ()
setVisualCodeImageSelector = mkSelector "setVisualCodeImage:"


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INListCarsIntentResponse@.
module ObjC.Intents.INListCarsIntentResponse
  ( INListCarsIntentResponse
  , IsINListCarsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , cars
  , setCars
  , carsSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setCarsSelector

  -- * Enum types
  , INListCarsIntentResponseCode(INListCarsIntentResponseCode)
  , pattern INListCarsIntentResponseCodeUnspecified
  , pattern INListCarsIntentResponseCodeReady
  , pattern INListCarsIntentResponseCodeInProgress
  , pattern INListCarsIntentResponseCodeSuccess
  , pattern INListCarsIntentResponseCodeFailure
  , pattern INListCarsIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO RawId
init_ inListCarsIntentResponse =
  sendOwnedMessage inListCarsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINListCarsIntentResponse inListCarsIntentResponse, IsNSUserActivity userActivity) => inListCarsIntentResponse -> INListCarsIntentResponseCode -> userActivity -> IO (Id INListCarsIntentResponse)
initWithCode_userActivity inListCarsIntentResponse code userActivity =
  sendOwnedMessage inListCarsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO INListCarsIntentResponseCode
code inListCarsIntentResponse =
  sendMessage inListCarsIntentResponse codeSelector

-- | @- cars@
cars :: IsINListCarsIntentResponse inListCarsIntentResponse => inListCarsIntentResponse -> IO (Id NSArray)
cars inListCarsIntentResponse =
  sendMessage inListCarsIntentResponse carsSelector

-- | @- setCars:@
setCars :: (IsINListCarsIntentResponse inListCarsIntentResponse, IsNSArray value) => inListCarsIntentResponse -> value -> IO ()
setCars inListCarsIntentResponse value =
  sendMessage inListCarsIntentResponse setCarsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INListCarsIntentResponseCode, Id NSUserActivity] (Id INListCarsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INListCarsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @cars@
carsSelector :: Selector '[] (Id NSArray)
carsSelector = mkSelector "cars"

-- | @Selector@ for @setCars:@
setCarsSelector :: Selector '[Id NSArray] ()
setCarsSelector = mkSelector "setCars:"


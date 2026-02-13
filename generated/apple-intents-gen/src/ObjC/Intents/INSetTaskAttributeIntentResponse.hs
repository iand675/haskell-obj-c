{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetTaskAttributeIntentResponse@.
module ObjC.Intents.INSetTaskAttributeIntentResponse
  ( INSetTaskAttributeIntentResponse
  , IsINSetTaskAttributeIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , modifiedTask
  , setModifiedTask
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , modifiedTaskSelector
  , setModifiedTaskSelector

  -- * Enum types
  , INSetTaskAttributeIntentResponseCode(INSetTaskAttributeIntentResponseCode)
  , pattern INSetTaskAttributeIntentResponseCodeUnspecified
  , pattern INSetTaskAttributeIntentResponseCodeReady
  , pattern INSetTaskAttributeIntentResponseCodeInProgress
  , pattern INSetTaskAttributeIntentResponseCodeSuccess
  , pattern INSetTaskAttributeIntentResponseCodeFailure
  , pattern INSetTaskAttributeIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO RawId
init_ inSetTaskAttributeIntentResponse =
  sendOwnedMessage inSetTaskAttributeIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse, IsNSUserActivity userActivity) => inSetTaskAttributeIntentResponse -> INSetTaskAttributeIntentResponseCode -> userActivity -> IO (Id INSetTaskAttributeIntentResponse)
initWithCode_userActivity inSetTaskAttributeIntentResponse code userActivity =
  sendOwnedMessage inSetTaskAttributeIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO INSetTaskAttributeIntentResponseCode
code inSetTaskAttributeIntentResponse =
  sendMessage inSetTaskAttributeIntentResponse codeSelector

-- | @- modifiedTask@
modifiedTask :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO (Id INTask)
modifiedTask inSetTaskAttributeIntentResponse =
  sendMessage inSetTaskAttributeIntentResponse modifiedTaskSelector

-- | @- setModifiedTask:@
setModifiedTask :: (IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse, IsINTask value) => inSetTaskAttributeIntentResponse -> value -> IO ()
setModifiedTask inSetTaskAttributeIntentResponse value =
  sendMessage inSetTaskAttributeIntentResponse setModifiedTaskSelector (toINTask value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetTaskAttributeIntentResponseCode, Id NSUserActivity] (Id INSetTaskAttributeIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetTaskAttributeIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @modifiedTask@
modifiedTaskSelector :: Selector '[] (Id INTask)
modifiedTaskSelector = mkSelector "modifiedTask"

-- | @Selector@ for @setModifiedTask:@
setModifiedTaskSelector :: Selector '[Id INTask] ()
setModifiedTaskSelector = mkSelector "setModifiedTask:"


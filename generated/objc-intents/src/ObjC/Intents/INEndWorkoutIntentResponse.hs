{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEndWorkoutIntentResponse@.
module ObjC.Intents.INEndWorkoutIntentResponse
  ( INEndWorkoutIntentResponse
  , IsINEndWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INEndWorkoutIntentResponseCode(INEndWorkoutIntentResponseCode)
  , pattern INEndWorkoutIntentResponseCodeUnspecified
  , pattern INEndWorkoutIntentResponseCodeReady
  , pattern INEndWorkoutIntentResponseCodeContinueInApp
  , pattern INEndWorkoutIntentResponseCodeFailure
  , pattern INEndWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INEndWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INEndWorkoutIntentResponseCodeHandleInApp
  , pattern INEndWorkoutIntentResponseCodeSuccess

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
init_ :: IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse => inEndWorkoutIntentResponse -> IO RawId
init_ inEndWorkoutIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inEndWorkoutIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse, IsNSUserActivity userActivity) => inEndWorkoutIntentResponse -> INEndWorkoutIntentResponseCode -> userActivity -> IO (Id INEndWorkoutIntentResponse)
initWithCode_userActivity inEndWorkoutIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inEndWorkoutIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse => inEndWorkoutIntentResponse -> IO INEndWorkoutIntentResponseCode
code inEndWorkoutIntentResponse  =
  fmap (coerce :: CLong -> INEndWorkoutIntentResponseCode) $ sendMsg inEndWorkoutIntentResponse (mkSelector "code") retCLong []

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


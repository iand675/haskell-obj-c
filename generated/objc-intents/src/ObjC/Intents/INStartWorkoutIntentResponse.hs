{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartWorkoutIntentResponse@.
module ObjC.Intents.INStartWorkoutIntentResponse
  ( INStartWorkoutIntentResponse
  , IsINStartWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INStartWorkoutIntentResponseCode(INStartWorkoutIntentResponseCode)
  , pattern INStartWorkoutIntentResponseCodeUnspecified
  , pattern INStartWorkoutIntentResponseCodeReady
  , pattern INStartWorkoutIntentResponseCodeContinueInApp
  , pattern INStartWorkoutIntentResponseCodeFailure
  , pattern INStartWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartWorkoutIntentResponseCodeFailureOngoingWorkout
  , pattern INStartWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INStartWorkoutIntentResponseCodeHandleInApp
  , pattern INStartWorkoutIntentResponseCodeSuccess

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
init_ :: IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse => inStartWorkoutIntentResponse -> IO RawId
init_ inStartWorkoutIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inStartWorkoutIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse, IsNSUserActivity userActivity) => inStartWorkoutIntentResponse -> INStartWorkoutIntentResponseCode -> userActivity -> IO (Id INStartWorkoutIntentResponse)
initWithCode_userActivity inStartWorkoutIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inStartWorkoutIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse => inStartWorkoutIntentResponse -> IO INStartWorkoutIntentResponseCode
code inStartWorkoutIntentResponse  =
  fmap (coerce :: CLong -> INStartWorkoutIntentResponseCode) $ sendMsg inStartWorkoutIntentResponse (mkSelector "code") retCLong []

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


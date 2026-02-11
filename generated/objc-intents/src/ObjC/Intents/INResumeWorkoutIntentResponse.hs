{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INResumeWorkoutIntentResponse@.
module ObjC.Intents.INResumeWorkoutIntentResponse
  ( INResumeWorkoutIntentResponse
  , IsINResumeWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INResumeWorkoutIntentResponseCode(INResumeWorkoutIntentResponseCode)
  , pattern INResumeWorkoutIntentResponseCodeUnspecified
  , pattern INResumeWorkoutIntentResponseCodeReady
  , pattern INResumeWorkoutIntentResponseCodeContinueInApp
  , pattern INResumeWorkoutIntentResponseCodeFailure
  , pattern INResumeWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INResumeWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INResumeWorkoutIntentResponseCodeHandleInApp
  , pattern INResumeWorkoutIntentResponseCodeSuccess

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
init_ :: IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse => inResumeWorkoutIntentResponse -> IO RawId
init_ inResumeWorkoutIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inResumeWorkoutIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse, IsNSUserActivity userActivity) => inResumeWorkoutIntentResponse -> INResumeWorkoutIntentResponseCode -> userActivity -> IO (Id INResumeWorkoutIntentResponse)
initWithCode_userActivity inResumeWorkoutIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inResumeWorkoutIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse => inResumeWorkoutIntentResponse -> IO INResumeWorkoutIntentResponseCode
code inResumeWorkoutIntentResponse  =
  fmap (coerce :: CLong -> INResumeWorkoutIntentResponseCode) $ sendMsg inResumeWorkoutIntentResponse (mkSelector "code") retCLong []

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


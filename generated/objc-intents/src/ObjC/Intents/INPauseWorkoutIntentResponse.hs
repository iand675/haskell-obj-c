{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPauseWorkoutIntentResponse@.
module ObjC.Intents.INPauseWorkoutIntentResponse
  ( INPauseWorkoutIntentResponse
  , IsINPauseWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INPauseWorkoutIntentResponseCode(INPauseWorkoutIntentResponseCode)
  , pattern INPauseWorkoutIntentResponseCodeUnspecified
  , pattern INPauseWorkoutIntentResponseCodeReady
  , pattern INPauseWorkoutIntentResponseCodeContinueInApp
  , pattern INPauseWorkoutIntentResponseCodeFailure
  , pattern INPauseWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INPauseWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INPauseWorkoutIntentResponseCodeHandleInApp
  , pattern INPauseWorkoutIntentResponseCodeSuccess

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
init_ :: IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse => inPauseWorkoutIntentResponse -> IO RawId
init_ inPauseWorkoutIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inPauseWorkoutIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse, IsNSUserActivity userActivity) => inPauseWorkoutIntentResponse -> INPauseWorkoutIntentResponseCode -> userActivity -> IO (Id INPauseWorkoutIntentResponse)
initWithCode_userActivity inPauseWorkoutIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inPauseWorkoutIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse => inPauseWorkoutIntentResponse -> IO INPauseWorkoutIntentResponseCode
code inPauseWorkoutIntentResponse  =
  fmap (coerce :: CLong -> INPauseWorkoutIntentResponseCode) $ sendMsg inPauseWorkoutIntentResponse (mkSelector "code") retCLong []

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


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCancelWorkoutIntentResponse@.
module ObjC.Intents.INCancelWorkoutIntentResponse
  ( INCancelWorkoutIntentResponse
  , IsINCancelWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INCancelWorkoutIntentResponseCode(INCancelWorkoutIntentResponseCode)
  , pattern INCancelWorkoutIntentResponseCodeUnspecified
  , pattern INCancelWorkoutIntentResponseCodeReady
  , pattern INCancelWorkoutIntentResponseCodeContinueInApp
  , pattern INCancelWorkoutIntentResponseCodeFailure
  , pattern INCancelWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INCancelWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INCancelWorkoutIntentResponseCodeHandleInApp
  , pattern INCancelWorkoutIntentResponseCodeSuccess

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
init_ :: IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse => inCancelWorkoutIntentResponse -> IO RawId
init_ inCancelWorkoutIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inCancelWorkoutIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse, IsNSUserActivity userActivity) => inCancelWorkoutIntentResponse -> INCancelWorkoutIntentResponseCode -> userActivity -> IO (Id INCancelWorkoutIntentResponse)
initWithCode_userActivity inCancelWorkoutIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inCancelWorkoutIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse => inCancelWorkoutIntentResponse -> IO INCancelWorkoutIntentResponseCode
code inCancelWorkoutIntentResponse  =
  fmap (coerce :: CLong -> INCancelWorkoutIntentResponseCode) $ sendMsg inCancelWorkoutIntentResponse (mkSelector "code") retCLong []

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


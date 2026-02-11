{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSnoozeTasksIntentResponse@.
module ObjC.Intents.INSnoozeTasksIntentResponse
  ( INSnoozeTasksIntentResponse
  , IsINSnoozeTasksIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , snoozedTasks
  , setSnoozedTasks
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , snoozedTasksSelector
  , setSnoozedTasksSelector

  -- * Enum types
  , INSnoozeTasksIntentResponseCode(INSnoozeTasksIntentResponseCode)
  , pattern INSnoozeTasksIntentResponseCodeUnspecified
  , pattern INSnoozeTasksIntentResponseCodeReady
  , pattern INSnoozeTasksIntentResponseCodeInProgress
  , pattern INSnoozeTasksIntentResponseCodeSuccess
  , pattern INSnoozeTasksIntentResponseCodeFailure
  , pattern INSnoozeTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO RawId
init_ inSnoozeTasksIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSnoozeTasksIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse, IsNSUserActivity userActivity) => inSnoozeTasksIntentResponse -> INSnoozeTasksIntentResponseCode -> userActivity -> IO (Id INSnoozeTasksIntentResponse)
initWithCode_userActivity inSnoozeTasksIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSnoozeTasksIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO INSnoozeTasksIntentResponseCode
code inSnoozeTasksIntentResponse  =
  fmap (coerce :: CLong -> INSnoozeTasksIntentResponseCode) $ sendMsg inSnoozeTasksIntentResponse (mkSelector "code") retCLong []

-- | @- snoozedTasks@
snoozedTasks :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO (Id NSArray)
snoozedTasks inSnoozeTasksIntentResponse  =
  sendMsg inSnoozeTasksIntentResponse (mkSelector "snoozedTasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnoozedTasks:@
setSnoozedTasks :: (IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse, IsNSArray value) => inSnoozeTasksIntentResponse -> value -> IO ()
setSnoozedTasks inSnoozeTasksIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSnoozeTasksIntentResponse (mkSelector "setSnoozedTasks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @snoozedTasks@
snoozedTasksSelector :: Selector
snoozedTasksSelector = mkSelector "snoozedTasks"

-- | @Selector@ for @setSnoozedTasks:@
setSnoozedTasksSelector :: Selector
setSnoozedTasksSelector = mkSelector "setSnoozedTasks:"


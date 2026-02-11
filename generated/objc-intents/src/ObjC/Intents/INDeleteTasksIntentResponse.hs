{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksIntentResponse@.
module ObjC.Intents.INDeleteTasksIntentResponse
  ( INDeleteTasksIntentResponse
  , IsINDeleteTasksIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , deletedTasks
  , setDeletedTasks
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , deletedTasksSelector
  , setDeletedTasksSelector

  -- * Enum types
  , INDeleteTasksIntentResponseCode(INDeleteTasksIntentResponseCode)
  , pattern INDeleteTasksIntentResponseCodeUnspecified
  , pattern INDeleteTasksIntentResponseCodeReady
  , pattern INDeleteTasksIntentResponseCodeInProgress
  , pattern INDeleteTasksIntentResponseCodeSuccess
  , pattern INDeleteTasksIntentResponseCodeFailure
  , pattern INDeleteTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO RawId
init_ inDeleteTasksIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inDeleteTasksIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse, IsNSUserActivity userActivity) => inDeleteTasksIntentResponse -> INDeleteTasksIntentResponseCode -> userActivity -> IO (Id INDeleteTasksIntentResponse)
initWithCode_userActivity inDeleteTasksIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inDeleteTasksIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO INDeleteTasksIntentResponseCode
code inDeleteTasksIntentResponse  =
  fmap (coerce :: CLong -> INDeleteTasksIntentResponseCode) $ sendMsg inDeleteTasksIntentResponse (mkSelector "code") retCLong []

-- | @- deletedTasks@
deletedTasks :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO (Id NSArray)
deletedTasks inDeleteTasksIntentResponse  =
  sendMsg inDeleteTasksIntentResponse (mkSelector "deletedTasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeletedTasks:@
setDeletedTasks :: (IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse, IsNSArray value) => inDeleteTasksIntentResponse -> value -> IO ()
setDeletedTasks inDeleteTasksIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inDeleteTasksIntentResponse (mkSelector "setDeletedTasks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @deletedTasks@
deletedTasksSelector :: Selector
deletedTasksSelector = mkSelector "deletedTasks"

-- | @Selector@ for @setDeletedTasks:@
setDeletedTasksSelector :: Selector
setDeletedTasksSelector = mkSelector "setDeletedTasks:"


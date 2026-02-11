{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateTaskListIntentResponse@.
module ObjC.Intents.INCreateTaskListIntentResponse
  ( INCreateTaskListIntentResponse
  , IsINCreateTaskListIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , createdTaskList
  , setCreatedTaskList
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , createdTaskListSelector
  , setCreatedTaskListSelector

  -- * Enum types
  , INCreateTaskListIntentResponseCode(INCreateTaskListIntentResponseCode)
  , pattern INCreateTaskListIntentResponseCodeUnspecified
  , pattern INCreateTaskListIntentResponseCodeReady
  , pattern INCreateTaskListIntentResponseCodeInProgress
  , pattern INCreateTaskListIntentResponseCodeSuccess
  , pattern INCreateTaskListIntentResponseCodeFailure
  , pattern INCreateTaskListIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO RawId
init_ inCreateTaskListIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inCreateTaskListIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse, IsNSUserActivity userActivity) => inCreateTaskListIntentResponse -> INCreateTaskListIntentResponseCode -> userActivity -> IO (Id INCreateTaskListIntentResponse)
initWithCode_userActivity inCreateTaskListIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inCreateTaskListIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO INCreateTaskListIntentResponseCode
code inCreateTaskListIntentResponse  =
  fmap (coerce :: CLong -> INCreateTaskListIntentResponseCode) $ sendMsg inCreateTaskListIntentResponse (mkSelector "code") retCLong []

-- | @- createdTaskList@
createdTaskList :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO (Id INTaskList)
createdTaskList inCreateTaskListIntentResponse  =
  sendMsg inCreateTaskListIntentResponse (mkSelector "createdTaskList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreatedTaskList:@
setCreatedTaskList :: (IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse, IsINTaskList value) => inCreateTaskListIntentResponse -> value -> IO ()
setCreatedTaskList inCreateTaskListIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inCreateTaskListIntentResponse (mkSelector "setCreatedTaskList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @createdTaskList@
createdTaskListSelector :: Selector
createdTaskListSelector = mkSelector "createdTaskList"

-- | @Selector@ for @setCreatedTaskList:@
setCreatedTaskListSelector :: Selector
setCreatedTaskListSelector = mkSelector "setCreatedTaskList:"


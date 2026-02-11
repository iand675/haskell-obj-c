{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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
init_ :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO RawId
init_ inSetTaskAttributeIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetTaskAttributeIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse, IsNSUserActivity userActivity) => inSetTaskAttributeIntentResponse -> INSetTaskAttributeIntentResponseCode -> userActivity -> IO (Id INSetTaskAttributeIntentResponse)
initWithCode_userActivity inSetTaskAttributeIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetTaskAttributeIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO INSetTaskAttributeIntentResponseCode
code inSetTaskAttributeIntentResponse  =
  fmap (coerce :: CLong -> INSetTaskAttributeIntentResponseCode) $ sendMsg inSetTaskAttributeIntentResponse (mkSelector "code") retCLong []

-- | @- modifiedTask@
modifiedTask :: IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse => inSetTaskAttributeIntentResponse -> IO (Id INTask)
modifiedTask inSetTaskAttributeIntentResponse  =
  sendMsg inSetTaskAttributeIntentResponse (mkSelector "modifiedTask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModifiedTask:@
setModifiedTask :: (IsINSetTaskAttributeIntentResponse inSetTaskAttributeIntentResponse, IsINTask value) => inSetTaskAttributeIntentResponse -> value -> IO ()
setModifiedTask inSetTaskAttributeIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSetTaskAttributeIntentResponse (mkSelector "setModifiedTask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @modifiedTask@
modifiedTaskSelector :: Selector
modifiedTaskSelector = mkSelector "modifiedTask"

-- | @Selector@ for @setModifiedTask:@
setModifiedTaskSelector :: Selector
setModifiedTaskSelector = mkSelector "setModifiedTask:"


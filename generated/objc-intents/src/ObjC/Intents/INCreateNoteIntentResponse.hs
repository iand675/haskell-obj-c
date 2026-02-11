{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateNoteIntentResponse@.
module ObjC.Intents.INCreateNoteIntentResponse
  ( INCreateNoteIntentResponse
  , IsINCreateNoteIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , createdNote
  , setCreatedNote
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , createdNoteSelector
  , setCreatedNoteSelector

  -- * Enum types
  , INCreateNoteIntentResponseCode(INCreateNoteIntentResponseCode)
  , pattern INCreateNoteIntentResponseCodeUnspecified
  , pattern INCreateNoteIntentResponseCodeReady
  , pattern INCreateNoteIntentResponseCodeInProgress
  , pattern INCreateNoteIntentResponseCodeSuccess
  , pattern INCreateNoteIntentResponseCodeFailure
  , pattern INCreateNoteIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO RawId
init_ inCreateNoteIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inCreateNoteIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCreateNoteIntentResponse inCreateNoteIntentResponse, IsNSUserActivity userActivity) => inCreateNoteIntentResponse -> INCreateNoteIntentResponseCode -> userActivity -> IO (Id INCreateNoteIntentResponse)
initWithCode_userActivity inCreateNoteIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inCreateNoteIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO INCreateNoteIntentResponseCode
code inCreateNoteIntentResponse  =
  fmap (coerce :: CLong -> INCreateNoteIntentResponseCode) $ sendMsg inCreateNoteIntentResponse (mkSelector "code") retCLong []

-- | @- createdNote@
createdNote :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO (Id INNote)
createdNote inCreateNoteIntentResponse  =
  sendMsg inCreateNoteIntentResponse (mkSelector "createdNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreatedNote:@
setCreatedNote :: (IsINCreateNoteIntentResponse inCreateNoteIntentResponse, IsINNote value) => inCreateNoteIntentResponse -> value -> IO ()
setCreatedNote inCreateNoteIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inCreateNoteIntentResponse (mkSelector "setCreatedNote:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @createdNote@
createdNoteSelector :: Selector
createdNoteSelector = mkSelector "createdNote"

-- | @Selector@ for @setCreatedNote:@
setCreatedNoteSelector :: Selector
setCreatedNoteSelector = mkSelector "setCreatedNote:"


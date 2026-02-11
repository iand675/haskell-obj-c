{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAppendToNoteIntentResponse@.
module ObjC.Intents.INAppendToNoteIntentResponse
  ( INAppendToNoteIntentResponse
  , IsINAppendToNoteIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , note
  , setNote
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , noteSelector
  , setNoteSelector

  -- * Enum types
  , INAppendToNoteIntentResponseCode(INAppendToNoteIntentResponseCode)
  , pattern INAppendToNoteIntentResponseCodeUnspecified
  , pattern INAppendToNoteIntentResponseCodeReady
  , pattern INAppendToNoteIntentResponseCodeInProgress
  , pattern INAppendToNoteIntentResponseCodeSuccess
  , pattern INAppendToNoteIntentResponseCodeFailure
  , pattern INAppendToNoteIntentResponseCodeFailureRequiringAppLaunch
  , pattern INAppendToNoteIntentResponseCodeFailureCannotUpdatePasswordProtectedNote

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
init_ :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO RawId
init_ inAppendToNoteIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inAppendToNoteIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse, IsNSUserActivity userActivity) => inAppendToNoteIntentResponse -> INAppendToNoteIntentResponseCode -> userActivity -> IO (Id INAppendToNoteIntentResponse)
initWithCode_userActivity inAppendToNoteIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inAppendToNoteIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO INAppendToNoteIntentResponseCode
code inAppendToNoteIntentResponse  =
  fmap (coerce :: CLong -> INAppendToNoteIntentResponseCode) $ sendMsg inAppendToNoteIntentResponse (mkSelector "code") retCLong []

-- | @- note@
note :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO (Id INNote)
note inAppendToNoteIntentResponse  =
  sendMsg inAppendToNoteIntentResponse (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNote:@
setNote :: (IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse, IsINNote value) => inAppendToNoteIntentResponse -> value -> IO ()
setNote inAppendToNoteIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inAppendToNoteIntentResponse (mkSelector "setNote:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

-- | @Selector@ for @setNote:@
setNoteSelector :: Selector
setNoteSelector = mkSelector "setNote:"


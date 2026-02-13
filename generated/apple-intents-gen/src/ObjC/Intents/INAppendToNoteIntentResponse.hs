{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO RawId
init_ inAppendToNoteIntentResponse =
  sendOwnedMessage inAppendToNoteIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse, IsNSUserActivity userActivity) => inAppendToNoteIntentResponse -> INAppendToNoteIntentResponseCode -> userActivity -> IO (Id INAppendToNoteIntentResponse)
initWithCode_userActivity inAppendToNoteIntentResponse code userActivity =
  sendOwnedMessage inAppendToNoteIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO INAppendToNoteIntentResponseCode
code inAppendToNoteIntentResponse =
  sendMessage inAppendToNoteIntentResponse codeSelector

-- | @- note@
note :: IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse => inAppendToNoteIntentResponse -> IO (Id INNote)
note inAppendToNoteIntentResponse =
  sendMessage inAppendToNoteIntentResponse noteSelector

-- | @- setNote:@
setNote :: (IsINAppendToNoteIntentResponse inAppendToNoteIntentResponse, IsINNote value) => inAppendToNoteIntentResponse -> value -> IO ()
setNote inAppendToNoteIntentResponse value =
  sendMessage inAppendToNoteIntentResponse setNoteSelector (toINNote value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INAppendToNoteIntentResponseCode, Id NSUserActivity] (Id INAppendToNoteIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INAppendToNoteIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id INNote)
noteSelector = mkSelector "note"

-- | @Selector@ for @setNote:@
setNoteSelector :: Selector '[Id INNote] ()
setNoteSelector = mkSelector "setNote:"


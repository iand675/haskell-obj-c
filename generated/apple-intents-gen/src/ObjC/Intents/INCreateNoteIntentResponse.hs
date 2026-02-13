{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , createdNoteSelector
  , initSelector
  , initWithCode_userActivitySelector
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
init_ :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO RawId
init_ inCreateNoteIntentResponse =
  sendOwnedMessage inCreateNoteIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCreateNoteIntentResponse inCreateNoteIntentResponse, IsNSUserActivity userActivity) => inCreateNoteIntentResponse -> INCreateNoteIntentResponseCode -> userActivity -> IO (Id INCreateNoteIntentResponse)
initWithCode_userActivity inCreateNoteIntentResponse code userActivity =
  sendOwnedMessage inCreateNoteIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO INCreateNoteIntentResponseCode
code inCreateNoteIntentResponse =
  sendMessage inCreateNoteIntentResponse codeSelector

-- | @- createdNote@
createdNote :: IsINCreateNoteIntentResponse inCreateNoteIntentResponse => inCreateNoteIntentResponse -> IO (Id INNote)
createdNote inCreateNoteIntentResponse =
  sendMessage inCreateNoteIntentResponse createdNoteSelector

-- | @- setCreatedNote:@
setCreatedNote :: (IsINCreateNoteIntentResponse inCreateNoteIntentResponse, IsINNote value) => inCreateNoteIntentResponse -> value -> IO ()
setCreatedNote inCreateNoteIntentResponse value =
  sendMessage inCreateNoteIntentResponse setCreatedNoteSelector (toINNote value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INCreateNoteIntentResponseCode, Id NSUserActivity] (Id INCreateNoteIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INCreateNoteIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @createdNote@
createdNoteSelector :: Selector '[] (Id INNote)
createdNoteSelector = mkSelector "createdNote"

-- | @Selector@ for @setCreatedNote:@
setCreatedNoteSelector :: Selector '[Id INNote] ()
setCreatedNoteSelector = mkSelector "setCreatedNote:"


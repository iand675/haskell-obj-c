{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAnswerCallIntentResponse@.
module ObjC.Intents.INAnswerCallIntentResponse
  ( INAnswerCallIntentResponse
  , IsINAnswerCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , callRecords
  , setCallRecords
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , callRecordsSelector
  , setCallRecordsSelector

  -- * Enum types
  , INAnswerCallIntentResponseCode(INAnswerCallIntentResponseCode)
  , pattern INAnswerCallIntentResponseCodeUnspecified
  , pattern INAnswerCallIntentResponseCodeReady
  , pattern INAnswerCallIntentResponseCodeContinueInApp
  , pattern INAnswerCallIntentResponseCodeInProgress
  , pattern INAnswerCallIntentResponseCodeSuccess
  , pattern INAnswerCallIntentResponseCodeFailure
  , pattern INAnswerCallIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO RawId
init_ inAnswerCallIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inAnswerCallIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAnswerCallIntentResponse inAnswerCallIntentResponse, IsNSUserActivity userActivity) => inAnswerCallIntentResponse -> INAnswerCallIntentResponseCode -> userActivity -> IO (Id INAnswerCallIntentResponse)
initWithCode_userActivity inAnswerCallIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inAnswerCallIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO INAnswerCallIntentResponseCode
code inAnswerCallIntentResponse  =
  fmap (coerce :: CLong -> INAnswerCallIntentResponseCode) $ sendMsg inAnswerCallIntentResponse (mkSelector "code") retCLong []

-- | @- callRecords@
callRecords :: IsINAnswerCallIntentResponse inAnswerCallIntentResponse => inAnswerCallIntentResponse -> IO (Id NSArray)
callRecords inAnswerCallIntentResponse  =
  sendMsg inAnswerCallIntentResponse (mkSelector "callRecords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCallRecords:@
setCallRecords :: (IsINAnswerCallIntentResponse inAnswerCallIntentResponse, IsNSArray value) => inAnswerCallIntentResponse -> value -> IO ()
setCallRecords inAnswerCallIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inAnswerCallIntentResponse (mkSelector "setCallRecords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @callRecords@
callRecordsSelector :: Selector
callRecordsSelector = mkSelector "callRecords"

-- | @Selector@ for @setCallRecords:@
setCallRecordsSelector :: Selector
setCallRecordsSelector = mkSelector "setCallRecords:"


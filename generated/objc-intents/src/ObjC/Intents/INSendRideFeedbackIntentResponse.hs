{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendRideFeedbackIntentResponse@.
module ObjC.Intents.INSendRideFeedbackIntentResponse
  ( INSendRideFeedbackIntentResponse
  , IsINSendRideFeedbackIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSendRideFeedbackIntentResponseCode(INSendRideFeedbackIntentResponseCode)
  , pattern INSendRideFeedbackIntentResponseCodeUnspecified
  , pattern INSendRideFeedbackIntentResponseCodeReady
  , pattern INSendRideFeedbackIntentResponseCodeSuccess
  , pattern INSendRideFeedbackIntentResponseCodeFailure

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
init_ :: IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse => inSendRideFeedbackIntentResponse -> IO RawId
init_ inSendRideFeedbackIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSendRideFeedbackIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse, IsNSUserActivity userActivity) => inSendRideFeedbackIntentResponse -> INSendRideFeedbackIntentResponseCode -> userActivity -> IO (Id INSendRideFeedbackIntentResponse)
initWithCode_userActivity inSendRideFeedbackIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSendRideFeedbackIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSendRideFeedbackIntentResponse inSendRideFeedbackIntentResponse => inSendRideFeedbackIntentResponse -> IO INSendRideFeedbackIntentResponseCode
code inSendRideFeedbackIntentResponse  =
  fmap (coerce :: CLong -> INSendRideFeedbackIntentResponseCode) $ sendMsg inSendRideFeedbackIntentResponse (mkSelector "code") retCLong []

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


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetMessageAttributeIntentResponse@.
module ObjC.Intents.INSetMessageAttributeIntentResponse
  ( INSetMessageAttributeIntentResponse
  , IsINSetMessageAttributeIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetMessageAttributeIntentResponseCode(INSetMessageAttributeIntentResponseCode)
  , pattern INSetMessageAttributeIntentResponseCodeUnspecified
  , pattern INSetMessageAttributeIntentResponseCodeReady
  , pattern INSetMessageAttributeIntentResponseCodeInProgress
  , pattern INSetMessageAttributeIntentResponseCodeSuccess
  , pattern INSetMessageAttributeIntentResponseCodeFailure
  , pattern INSetMessageAttributeIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSetMessageAttributeIntentResponseCodeFailureMessageNotFound
  , pattern INSetMessageAttributeIntentResponseCodeFailureMessageAttributeNotSet

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
init_ :: IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse => inSetMessageAttributeIntentResponse -> IO RawId
init_ inSetMessageAttributeIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetMessageAttributeIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse, IsNSUserActivity userActivity) => inSetMessageAttributeIntentResponse -> INSetMessageAttributeIntentResponseCode -> userActivity -> IO (Id INSetMessageAttributeIntentResponse)
initWithCode_userActivity inSetMessageAttributeIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetMessageAttributeIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse => inSetMessageAttributeIntentResponse -> IO INSetMessageAttributeIntentResponseCode
code inSetMessageAttributeIntentResponse  =
  fmap (coerce :: CLong -> INSetMessageAttributeIntentResponseCode) $ sendMsg inSetMessageAttributeIntentResponse (mkSelector "code") retCLong []

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


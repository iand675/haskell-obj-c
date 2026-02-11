{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INShareFocusStatusIntentResponse@.
module ObjC.Intents.INShareFocusStatusIntentResponse
  ( INShareFocusStatusIntentResponse
  , IsINShareFocusStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INShareFocusStatusIntentResponseCode(INShareFocusStatusIntentResponseCode)
  , pattern INShareFocusStatusIntentResponseCodeUnspecified
  , pattern INShareFocusStatusIntentResponseCodeReady
  , pattern INShareFocusStatusIntentResponseCodeInProgress
  , pattern INShareFocusStatusIntentResponseCodeSuccess
  , pattern INShareFocusStatusIntentResponseCodeFailure
  , pattern INShareFocusStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse => inShareFocusStatusIntentResponse -> IO RawId
init_ inShareFocusStatusIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inShareFocusStatusIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse, IsNSUserActivity userActivity) => inShareFocusStatusIntentResponse -> INShareFocusStatusIntentResponseCode -> userActivity -> IO (Id INShareFocusStatusIntentResponse)
initWithCode_userActivity inShareFocusStatusIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inShareFocusStatusIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINShareFocusStatusIntentResponse inShareFocusStatusIntentResponse => inShareFocusStatusIntentResponse -> IO INShareFocusStatusIntentResponseCode
code inShareFocusStatusIntentResponse  =
  fmap (coerce :: CLong -> INShareFocusStatusIntentResponseCode) $ sendMsg inShareFocusStatusIntentResponse (mkSelector "code") retCLong []

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


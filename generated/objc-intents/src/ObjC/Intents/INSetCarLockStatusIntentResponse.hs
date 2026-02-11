{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetCarLockStatusIntentResponse@.
module ObjC.Intents.INSetCarLockStatusIntentResponse
  ( INSetCarLockStatusIntentResponse
  , IsINSetCarLockStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetCarLockStatusIntentResponseCode(INSetCarLockStatusIntentResponseCode)
  , pattern INSetCarLockStatusIntentResponseCodeUnspecified
  , pattern INSetCarLockStatusIntentResponseCodeReady
  , pattern INSetCarLockStatusIntentResponseCodeInProgress
  , pattern INSetCarLockStatusIntentResponseCodeSuccess
  , pattern INSetCarLockStatusIntentResponseCodeFailure
  , pattern INSetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse => inSetCarLockStatusIntentResponse -> IO RawId
init_ inSetCarLockStatusIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetCarLockStatusIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse, IsNSUserActivity userActivity) => inSetCarLockStatusIntentResponse -> INSetCarLockStatusIntentResponseCode -> userActivity -> IO (Id INSetCarLockStatusIntentResponse)
initWithCode_userActivity inSetCarLockStatusIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetCarLockStatusIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetCarLockStatusIntentResponse inSetCarLockStatusIntentResponse => inSetCarLockStatusIntentResponse -> IO INSetCarLockStatusIntentResponseCode
code inSetCarLockStatusIntentResponse  =
  fmap (coerce :: CLong -> INSetCarLockStatusIntentResponseCode) $ sendMsg inSetCarLockStatusIntentResponse (mkSelector "code") retCLong []

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


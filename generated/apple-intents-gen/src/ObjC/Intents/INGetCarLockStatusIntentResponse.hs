{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarLockStatusIntentResponse@.
module ObjC.Intents.INGetCarLockStatusIntentResponse
  ( INGetCarLockStatusIntentResponse
  , IsINGetCarLockStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , locked
  , setLocked
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , lockedSelector
  , setLockedSelector

  -- * Enum types
  , INGetCarLockStatusIntentResponseCode(INGetCarLockStatusIntentResponseCode)
  , pattern INGetCarLockStatusIntentResponseCodeUnspecified
  , pattern INGetCarLockStatusIntentResponseCodeReady
  , pattern INGetCarLockStatusIntentResponseCodeInProgress
  , pattern INGetCarLockStatusIntentResponseCodeSuccess
  , pattern INGetCarLockStatusIntentResponseCodeFailure
  , pattern INGetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO RawId
init_ inGetCarLockStatusIntentResponse  =
    fmap (RawId . castPtr) $ sendMsg inGetCarLockStatusIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse, IsNSUserActivity userActivity) => inGetCarLockStatusIntentResponse -> INGetCarLockStatusIntentResponseCode -> userActivity -> IO (Id INGetCarLockStatusIntentResponse)
initWithCode_userActivity inGetCarLockStatusIntentResponse  code userActivity =
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inGetCarLockStatusIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO INGetCarLockStatusIntentResponseCode
code inGetCarLockStatusIntentResponse  =
    fmap (coerce :: CLong -> INGetCarLockStatusIntentResponseCode) $ sendMsg inGetCarLockStatusIntentResponse (mkSelector "code") retCLong []

-- | @- locked@
locked :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO (Id NSNumber)
locked inGetCarLockStatusIntentResponse  =
    sendMsg inGetCarLockStatusIntentResponse (mkSelector "locked") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocked:@
setLocked :: (IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse, IsNSNumber value) => inGetCarLockStatusIntentResponse -> value -> IO ()
setLocked inGetCarLockStatusIntentResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inGetCarLockStatusIntentResponse (mkSelector "setLocked:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @locked@
lockedSelector :: Selector
lockedSelector = mkSelector "locked"

-- | @Selector@ for @setLocked:@
setLockedSelector :: Selector
setLockedSelector = mkSelector "setLocked:"


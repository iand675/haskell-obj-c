{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetProfileInCarIntentResponse@.
module ObjC.Intents.INSetProfileInCarIntentResponse
  ( INSetProfileInCarIntentResponse
  , IsINSetProfileInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetProfileInCarIntentResponseCode(INSetProfileInCarIntentResponseCode)
  , pattern INSetProfileInCarIntentResponseCodeUnspecified
  , pattern INSetProfileInCarIntentResponseCodeReady
  , pattern INSetProfileInCarIntentResponseCodeInProgress
  , pattern INSetProfileInCarIntentResponseCodeSuccess
  , pattern INSetProfileInCarIntentResponseCodeFailure
  , pattern INSetProfileInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse => inSetProfileInCarIntentResponse -> IO RawId
init_ inSetProfileInCarIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetProfileInCarIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse, IsNSUserActivity userActivity) => inSetProfileInCarIntentResponse -> INSetProfileInCarIntentResponseCode -> userActivity -> IO (Id INSetProfileInCarIntentResponse)
initWithCode_userActivity inSetProfileInCarIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetProfileInCarIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse => inSetProfileInCarIntentResponse -> IO INSetProfileInCarIntentResponseCode
code inSetProfileInCarIntentResponse  =
  fmap (coerce :: CLong -> INSetProfileInCarIntentResponseCode) $ sendMsg inSetProfileInCarIntentResponse (mkSelector "code") retCLong []

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


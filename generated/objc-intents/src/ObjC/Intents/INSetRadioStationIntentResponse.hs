{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetRadioStationIntentResponse@.
module ObjC.Intents.INSetRadioStationIntentResponse
  ( INSetRadioStationIntentResponse
  , IsINSetRadioStationIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector

  -- * Enum types
  , INSetRadioStationIntentResponseCode(INSetRadioStationIntentResponseCode)
  , pattern INSetRadioStationIntentResponseCodeUnspecified
  , pattern INSetRadioStationIntentResponseCodeReady
  , pattern INSetRadioStationIntentResponseCodeInProgress
  , pattern INSetRadioStationIntentResponseCodeSuccess
  , pattern INSetRadioStationIntentResponseCodeFailure
  , pattern INSetRadioStationIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSetRadioStationIntentResponseCodeFailureNotSubscribed

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
init_ :: IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse => inSetRadioStationIntentResponse -> IO RawId
init_ inSetRadioStationIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSetRadioStationIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse, IsNSUserActivity userActivity) => inSetRadioStationIntentResponse -> INSetRadioStationIntentResponseCode -> userActivity -> IO (Id INSetRadioStationIntentResponse)
initWithCode_userActivity inSetRadioStationIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSetRadioStationIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSetRadioStationIntentResponse inSetRadioStationIntentResponse => inSetRadioStationIntentResponse -> IO INSetRadioStationIntentResponseCode
code inSetRadioStationIntentResponse  =
  fmap (coerce :: CLong -> INSetRadioStationIntentResponseCode) $ sendMsg inSetRadioStationIntentResponse (mkSelector "code") retCLong []

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


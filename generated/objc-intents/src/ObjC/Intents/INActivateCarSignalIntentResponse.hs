{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INActivateCarSignalIntentResponse@.
module ObjC.Intents.INActivateCarSignalIntentResponse
  ( INActivateCarSignalIntentResponse
  , IsINActivateCarSignalIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , signals
  , setSignals
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , signalsSelector
  , setSignalsSelector

  -- * Enum types
  , INActivateCarSignalIntentResponseCode(INActivateCarSignalIntentResponseCode)
  , pattern INActivateCarSignalIntentResponseCodeUnspecified
  , pattern INActivateCarSignalIntentResponseCodeReady
  , pattern INActivateCarSignalIntentResponseCodeInProgress
  , pattern INActivateCarSignalIntentResponseCodeSuccess
  , pattern INActivateCarSignalIntentResponseCodeFailure
  , pattern INActivateCarSignalIntentResponseCodeFailureRequiringAppLaunch
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

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
init_ :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO RawId
init_ inActivateCarSignalIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inActivateCarSignalIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse, IsNSUserActivity userActivity) => inActivateCarSignalIntentResponse -> INActivateCarSignalIntentResponseCode -> userActivity -> IO (Id INActivateCarSignalIntentResponse)
initWithCode_userActivity inActivateCarSignalIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inActivateCarSignalIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO INActivateCarSignalIntentResponseCode
code inActivateCarSignalIntentResponse  =
  fmap (coerce :: CLong -> INActivateCarSignalIntentResponseCode) $ sendMsg inActivateCarSignalIntentResponse (mkSelector "code") retCLong []

-- | @- signals@
signals :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> IO INCarSignalOptions
signals inActivateCarSignalIntentResponse  =
  fmap (coerce :: CULong -> INCarSignalOptions) $ sendMsg inActivateCarSignalIntentResponse (mkSelector "signals") retCULong []

-- | @- setSignals:@
setSignals :: IsINActivateCarSignalIntentResponse inActivateCarSignalIntentResponse => inActivateCarSignalIntentResponse -> INCarSignalOptions -> IO ()
setSignals inActivateCarSignalIntentResponse  value =
  sendMsg inActivateCarSignalIntentResponse (mkSelector "setSignals:") retVoid [argCULong (coerce value)]

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

-- | @Selector@ for @signals@
signalsSelector :: Selector
signalsSelector = mkSelector "signals"

-- | @Selector@ for @setSignals:@
setSignalsSelector :: Selector
setSignalsSelector = mkSelector "setSignals:"


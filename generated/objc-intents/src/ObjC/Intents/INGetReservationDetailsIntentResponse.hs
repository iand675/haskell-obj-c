{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetReservationDetailsIntentResponse@.
module ObjC.Intents.INGetReservationDetailsIntentResponse
  ( INGetReservationDetailsIntentResponse
  , IsINGetReservationDetailsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , reservations
  , setReservations
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , reservationsSelector
  , setReservationsSelector

  -- * Enum types
  , INGetReservationDetailsIntentResponseCode(INGetReservationDetailsIntentResponseCode)
  , pattern INGetReservationDetailsIntentResponseCodeUnspecified
  , pattern INGetReservationDetailsIntentResponseCodeReady
  , pattern INGetReservationDetailsIntentResponseCodeInProgress
  , pattern INGetReservationDetailsIntentResponseCodeSuccess
  , pattern INGetReservationDetailsIntentResponseCodeFailure
  , pattern INGetReservationDetailsIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO RawId
init_ inGetReservationDetailsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inGetReservationDetailsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse, IsNSUserActivity userActivity) => inGetReservationDetailsIntentResponse -> INGetReservationDetailsIntentResponseCode -> userActivity -> IO (Id INGetReservationDetailsIntentResponse)
initWithCode_userActivity inGetReservationDetailsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inGetReservationDetailsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO INGetReservationDetailsIntentResponseCode
code inGetReservationDetailsIntentResponse  =
  fmap (coerce :: CLong -> INGetReservationDetailsIntentResponseCode) $ sendMsg inGetReservationDetailsIntentResponse (mkSelector "code") retCLong []

-- | @- reservations@
reservations :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO (Id NSArray)
reservations inGetReservationDetailsIntentResponse  =
  sendMsg inGetReservationDetailsIntentResponse (mkSelector "reservations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReservations:@
setReservations :: (IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse, IsNSArray value) => inGetReservationDetailsIntentResponse -> value -> IO ()
setReservations inGetReservationDetailsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetReservationDetailsIntentResponse (mkSelector "setReservations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @reservations@
reservationsSelector :: Selector
reservationsSelector = mkSelector "reservations"

-- | @Selector@ for @setReservations:@
setReservationsSelector :: Selector
setReservationsSelector = mkSelector "setReservations:"


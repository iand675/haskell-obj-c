{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO RawId
init_ inGetReservationDetailsIntentResponse =
  sendOwnedMessage inGetReservationDetailsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse, IsNSUserActivity userActivity) => inGetReservationDetailsIntentResponse -> INGetReservationDetailsIntentResponseCode -> userActivity -> IO (Id INGetReservationDetailsIntentResponse)
initWithCode_userActivity inGetReservationDetailsIntentResponse code userActivity =
  sendOwnedMessage inGetReservationDetailsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO INGetReservationDetailsIntentResponseCode
code inGetReservationDetailsIntentResponse =
  sendMessage inGetReservationDetailsIntentResponse codeSelector

-- | @- reservations@
reservations :: IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse => inGetReservationDetailsIntentResponse -> IO (Id NSArray)
reservations inGetReservationDetailsIntentResponse =
  sendMessage inGetReservationDetailsIntentResponse reservationsSelector

-- | @- setReservations:@
setReservations :: (IsINGetReservationDetailsIntentResponse inGetReservationDetailsIntentResponse, IsNSArray value) => inGetReservationDetailsIntentResponse -> value -> IO ()
setReservations inGetReservationDetailsIntentResponse value =
  sendMessage inGetReservationDetailsIntentResponse setReservationsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetReservationDetailsIntentResponseCode, Id NSUserActivity] (Id INGetReservationDetailsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetReservationDetailsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @reservations@
reservationsSelector :: Selector '[] (Id NSArray)
reservationsSelector = mkSelector "reservations"

-- | @Selector@ for @setReservations:@
setReservationsSelector :: Selector '[Id NSArray] ()
setReservationsSelector = mkSelector "setReservations:"


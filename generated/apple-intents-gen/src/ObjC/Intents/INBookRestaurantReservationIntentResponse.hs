{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBookRestaurantReservationIntentResponse@.
module ObjC.Intents.INBookRestaurantReservationIntentResponse
  ( INBookRestaurantReservationIntentResponse
  , IsINBookRestaurantReservationIntentResponse(..)
  , initWithCode_userActivity
  , code
  , userBooking
  , setUserBooking
  , codeSelector
  , initWithCode_userActivitySelector
  , setUserBookingSelector
  , userBookingSelector

  -- * Enum types
  , INBookRestaurantReservationIntentCode(INBookRestaurantReservationIntentCode)
  , pattern INBookRestaurantReservationIntentCodeSuccess
  , pattern INBookRestaurantReservationIntentCodeDenied
  , pattern INBookRestaurantReservationIntentCodeFailure
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunch
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchMustVerifyCredentials
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable

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

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse, IsNSUserActivity userActivity) => inBookRestaurantReservationIntentResponse -> INBookRestaurantReservationIntentCode -> userActivity -> IO (Id INBookRestaurantReservationIntentResponse)
initWithCode_userActivity inBookRestaurantReservationIntentResponse code userActivity =
  sendOwnedMessage inBookRestaurantReservationIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse => inBookRestaurantReservationIntentResponse -> IO INBookRestaurantReservationIntentCode
code inBookRestaurantReservationIntentResponse =
  sendMessage inBookRestaurantReservationIntentResponse codeSelector

-- | @- userBooking@
userBooking :: IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse => inBookRestaurantReservationIntentResponse -> IO (Id INRestaurantReservationUserBooking)
userBooking inBookRestaurantReservationIntentResponse =
  sendMessage inBookRestaurantReservationIntentResponse userBookingSelector

-- | @- setUserBooking:@
setUserBooking :: (IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse, IsINRestaurantReservationUserBooking value) => inBookRestaurantReservationIntentResponse -> value -> IO ()
setUserBooking inBookRestaurantReservationIntentResponse value =
  sendMessage inBookRestaurantReservationIntentResponse setUserBookingSelector (toINRestaurantReservationUserBooking value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INBookRestaurantReservationIntentCode, Id NSUserActivity] (Id INBookRestaurantReservationIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INBookRestaurantReservationIntentCode
codeSelector = mkSelector "code"

-- | @Selector@ for @userBooking@
userBookingSelector :: Selector '[] (Id INRestaurantReservationUserBooking)
userBookingSelector = mkSelector "userBooking"

-- | @Selector@ for @setUserBooking:@
setUserBookingSelector :: Selector '[Id INRestaurantReservationUserBooking] ()
setUserBookingSelector = mkSelector "setUserBooking:"


{-# LANGUAGE PatternSynonyms #-}
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
  , initWithCode_userActivitySelector
  , codeSelector
  , userBookingSelector
  , setUserBookingSelector

  -- * Enum types
  , INBookRestaurantReservationIntentCode(INBookRestaurantReservationIntentCode)
  , pattern INBookRestaurantReservationIntentCodeSuccess
  , pattern INBookRestaurantReservationIntentCodeDenied
  , pattern INBookRestaurantReservationIntentCodeFailure
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunch
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchMustVerifyCredentials
  , pattern INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable

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

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse, IsNSUserActivity userActivity) => inBookRestaurantReservationIntentResponse -> INBookRestaurantReservationIntentCode -> userActivity -> IO (Id INBookRestaurantReservationIntentResponse)
initWithCode_userActivity inBookRestaurantReservationIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inBookRestaurantReservationIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse => inBookRestaurantReservationIntentResponse -> IO INBookRestaurantReservationIntentCode
code inBookRestaurantReservationIntentResponse  =
  fmap (coerce :: CLong -> INBookRestaurantReservationIntentCode) $ sendMsg inBookRestaurantReservationIntentResponse (mkSelector "code") retCLong []

-- | @- userBooking@
userBooking :: IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse => inBookRestaurantReservationIntentResponse -> IO (Id INRestaurantReservationUserBooking)
userBooking inBookRestaurantReservationIntentResponse  =
  sendMsg inBookRestaurantReservationIntentResponse (mkSelector "userBooking") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserBooking:@
setUserBooking :: (IsINBookRestaurantReservationIntentResponse inBookRestaurantReservationIntentResponse, IsINRestaurantReservationUserBooking value) => inBookRestaurantReservationIntentResponse -> value -> IO ()
setUserBooking inBookRestaurantReservationIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntentResponse (mkSelector "setUserBooking:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @userBooking@
userBookingSelector :: Selector
userBookingSelector = mkSelector "userBooking"

-- | @Selector@ for @setUserBooking:@
setUserBookingSelector :: Selector
setUserBookingSelector = mkSelector "setUserBooking:"


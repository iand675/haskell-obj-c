{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingsIntentResponse@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingsIntentResponse
  ( INGetAvailableRestaurantReservationBookingsIntentResponse
  , IsINGetAvailableRestaurantReservationBookingsIntentResponse(..)
  , initWithAvailableBookings_code_userActivity
  , code
  , localizedRestaurantDescriptionText
  , setLocalizedRestaurantDescriptionText
  , localizedBookingAdvisementText
  , setLocalizedBookingAdvisementText
  , termsAndConditions
  , setTermsAndConditions
  , availableBookings
  , initWithAvailableBookings_code_userActivitySelector
  , codeSelector
  , localizedRestaurantDescriptionTextSelector
  , setLocalizedRestaurantDescriptionTextSelector
  , localizedBookingAdvisementTextSelector
  , setLocalizedBookingAdvisementTextSelector
  , termsAndConditionsSelector
  , setTermsAndConditionsSelector
  , availableBookingsSelector

  -- * Enum types
  , INGetAvailableRestaurantReservationBookingsIntentCode(INGetAvailableRestaurantReservationBookingsIntentCode)
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeSuccess
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailure
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnsatisfiable
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnspecified

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

-- | @- initWithAvailableBookings:code:userActivity:@
initWithAvailableBookings_code_userActivity :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSArray availableBookings, IsNSUserActivity userActivity) => inGetAvailableRestaurantReservationBookingsIntentResponse -> availableBookings -> INGetAvailableRestaurantReservationBookingsIntentCode -> userActivity -> IO (Id INGetAvailableRestaurantReservationBookingsIntentResponse)
initWithAvailableBookings_code_userActivity inGetAvailableRestaurantReservationBookingsIntentResponse  availableBookings code userActivity =
withObjCPtr availableBookings $ \raw_availableBookings ->
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "initWithAvailableBookings:code:userActivity:") (retPtr retVoid) [argPtr (castPtr raw_availableBookings :: Ptr ()), argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO INGetAvailableRestaurantReservationBookingsIntentCode
code inGetAvailableRestaurantReservationBookingsIntentResponse  =
  fmap (coerce :: CLong -> INGetAvailableRestaurantReservationBookingsIntentCode) $ sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "code") retCLong []

-- | @- localizedRestaurantDescriptionText@
localizedRestaurantDescriptionText :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSString)
localizedRestaurantDescriptionText inGetAvailableRestaurantReservationBookingsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "localizedRestaurantDescriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedRestaurantDescriptionText:@
setLocalizedRestaurantDescriptionText :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSString value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setLocalizedRestaurantDescriptionText inGetAvailableRestaurantReservationBookingsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "setLocalizedRestaurantDescriptionText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedBookingAdvisementText@
localizedBookingAdvisementText :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSString)
localizedBookingAdvisementText inGetAvailableRestaurantReservationBookingsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "localizedBookingAdvisementText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedBookingAdvisementText:@
setLocalizedBookingAdvisementText :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSString value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setLocalizedBookingAdvisementText inGetAvailableRestaurantReservationBookingsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "setLocalizedBookingAdvisementText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- termsAndConditions@
termsAndConditions :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id INTermsAndConditions)
termsAndConditions inGetAvailableRestaurantReservationBookingsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "termsAndConditions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTermsAndConditions:@
setTermsAndConditions :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsINTermsAndConditions value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setTermsAndConditions inGetAvailableRestaurantReservationBookingsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "setTermsAndConditions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- availableBookings@
availableBookings :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSArray)
availableBookings inGetAvailableRestaurantReservationBookingsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingsIntentResponse (mkSelector "availableBookings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAvailableBookings:code:userActivity:@
initWithAvailableBookings_code_userActivitySelector :: Selector
initWithAvailableBookings_code_userActivitySelector = mkSelector "initWithAvailableBookings:code:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @localizedRestaurantDescriptionText@
localizedRestaurantDescriptionTextSelector :: Selector
localizedRestaurantDescriptionTextSelector = mkSelector "localizedRestaurantDescriptionText"

-- | @Selector@ for @setLocalizedRestaurantDescriptionText:@
setLocalizedRestaurantDescriptionTextSelector :: Selector
setLocalizedRestaurantDescriptionTextSelector = mkSelector "setLocalizedRestaurantDescriptionText:"

-- | @Selector@ for @localizedBookingAdvisementText@
localizedBookingAdvisementTextSelector :: Selector
localizedBookingAdvisementTextSelector = mkSelector "localizedBookingAdvisementText"

-- | @Selector@ for @setLocalizedBookingAdvisementText:@
setLocalizedBookingAdvisementTextSelector :: Selector
setLocalizedBookingAdvisementTextSelector = mkSelector "setLocalizedBookingAdvisementText:"

-- | @Selector@ for @termsAndConditions@
termsAndConditionsSelector :: Selector
termsAndConditionsSelector = mkSelector "termsAndConditions"

-- | @Selector@ for @setTermsAndConditions:@
setTermsAndConditionsSelector :: Selector
setTermsAndConditionsSelector = mkSelector "setTermsAndConditions:"

-- | @Selector@ for @availableBookings@
availableBookingsSelector :: Selector
availableBookingsSelector = mkSelector "availableBookings"


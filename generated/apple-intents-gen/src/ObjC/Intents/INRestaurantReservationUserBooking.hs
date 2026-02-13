{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantReservationUserBooking@.
module ObjC.Intents.INRestaurantReservationUserBooking
  ( INRestaurantReservationUserBooking
  , IsINRestaurantReservationUserBooking(..)
  , initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModified
  , guest
  , setGuest
  , advisementText
  , setAdvisementText
  , selectedOffer
  , setSelectedOffer
  , guestProvidedSpecialRequestText
  , setGuestProvidedSpecialRequestText
  , status
  , setStatus
  , dateStatusModified
  , setDateStatusModified
  , advisementTextSelector
  , dateStatusModifiedSelector
  , guestProvidedSpecialRequestTextSelector
  , guestSelector
  , initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector
  , selectedOfferSelector
  , setAdvisementTextSelector
  , setDateStatusModifiedSelector
  , setGuestProvidedSpecialRequestTextSelector
  , setGuestSelector
  , setSelectedOfferSelector
  , setStatusSelector
  , statusSelector

  -- * Enum types
  , INRestaurantReservationUserBookingStatus(INRestaurantReservationUserBookingStatus)
  , pattern INRestaurantReservationUserBookingStatusPending
  , pattern INRestaurantReservationUserBookingStatusConfirmed
  , pattern INRestaurantReservationUserBookingStatusDenied

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

-- | @- initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModified :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurant restaurant, IsNSDate bookingDate, IsNSString bookingIdentifier, IsINRestaurantGuest guest, IsNSDate dateStatusModified) => inRestaurantReservationUserBooking -> restaurant -> bookingDate -> CULong -> bookingIdentifier -> guest -> INRestaurantReservationUserBookingStatus -> dateStatusModified -> IO (Id INRestaurantReservationUserBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModified inRestaurantReservationUserBooking restaurant bookingDate partySize bookingIdentifier guest status dateStatusModified =
  sendOwnedMessage inRestaurantReservationUserBooking initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector (toINRestaurant restaurant) (toNSDate bookingDate) partySize (toNSString bookingIdentifier) (toINRestaurantGuest guest) status (toNSDate dateStatusModified)

-- | @- guest@
guest :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id INRestaurantGuest)
guest inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking guestSelector

-- | @- setGuest:@
setGuest :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurantGuest value) => inRestaurantReservationUserBooking -> value -> IO ()
setGuest inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setGuestSelector (toINRestaurantGuest value)

-- | @- advisementText@
advisementText :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSString)
advisementText inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking advisementTextSelector

-- | @- setAdvisementText:@
setAdvisementText :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSString value) => inRestaurantReservationUserBooking -> value -> IO ()
setAdvisementText inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setAdvisementTextSelector (toNSString value)

-- | @- selectedOffer@
selectedOffer :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id INRestaurantOffer)
selectedOffer inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking selectedOfferSelector

-- | @- setSelectedOffer:@
setSelectedOffer :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurantOffer value) => inRestaurantReservationUserBooking -> value -> IO ()
setSelectedOffer inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setSelectedOfferSelector (toINRestaurantOffer value)

-- | @- guestProvidedSpecialRequestText@
guestProvidedSpecialRequestText :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSString)
guestProvidedSpecialRequestText inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking guestProvidedSpecialRequestTextSelector

-- | @- setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestText :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSString value) => inRestaurantReservationUserBooking -> value -> IO ()
setGuestProvidedSpecialRequestText inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setGuestProvidedSpecialRequestTextSelector (toNSString value)

-- | @- status@
status :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO INRestaurantReservationUserBookingStatus
status inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking statusSelector

-- | @- setStatus:@
setStatus :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> INRestaurantReservationUserBookingStatus -> IO ()
setStatus inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setStatusSelector value

-- | @- dateStatusModified@
dateStatusModified :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSDate)
dateStatusModified inRestaurantReservationUserBooking =
  sendMessage inRestaurantReservationUserBooking dateStatusModifiedSelector

-- | @- setDateStatusModified:@
setDateStatusModified :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSDate value) => inRestaurantReservationUserBooking -> value -> IO ()
setDateStatusModified inRestaurantReservationUserBooking value =
  sendMessage inRestaurantReservationUserBooking setDateStatusModifiedSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector :: Selector '[Id INRestaurant, Id NSDate, CULong, Id NSString, Id INRestaurantGuest, INRestaurantReservationUserBookingStatus, Id NSDate] (Id INRestaurantReservationUserBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector = mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:"

-- | @Selector@ for @guest@
guestSelector :: Selector '[] (Id INRestaurantGuest)
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector '[Id INRestaurantGuest] ()
setGuestSelector = mkSelector "setGuest:"

-- | @Selector@ for @advisementText@
advisementTextSelector :: Selector '[] (Id NSString)
advisementTextSelector = mkSelector "advisementText"

-- | @Selector@ for @setAdvisementText:@
setAdvisementTextSelector :: Selector '[Id NSString] ()
setAdvisementTextSelector = mkSelector "setAdvisementText:"

-- | @Selector@ for @selectedOffer@
selectedOfferSelector :: Selector '[] (Id INRestaurantOffer)
selectedOfferSelector = mkSelector "selectedOffer"

-- | @Selector@ for @setSelectedOffer:@
setSelectedOfferSelector :: Selector '[Id INRestaurantOffer] ()
setSelectedOfferSelector = mkSelector "setSelectedOffer:"

-- | @Selector@ for @guestProvidedSpecialRequestText@
guestProvidedSpecialRequestTextSelector :: Selector '[] (Id NSString)
guestProvidedSpecialRequestTextSelector = mkSelector "guestProvidedSpecialRequestText"

-- | @Selector@ for @setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestTextSelector :: Selector '[Id NSString] ()
setGuestProvidedSpecialRequestTextSelector = mkSelector "setGuestProvidedSpecialRequestText:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INRestaurantReservationUserBookingStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[INRestaurantReservationUserBookingStatus] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @dateStatusModified@
dateStatusModifiedSelector :: Selector '[] (Id NSDate)
dateStatusModifiedSelector = mkSelector "dateStatusModified"

-- | @Selector@ for @setDateStatusModified:@
setDateStatusModifiedSelector :: Selector '[Id NSDate] ()
setDateStatusModifiedSelector = mkSelector "setDateStatusModified:"


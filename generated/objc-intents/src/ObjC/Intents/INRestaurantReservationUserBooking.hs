{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector
  , guestSelector
  , setGuestSelector
  , advisementTextSelector
  , setAdvisementTextSelector
  , selectedOfferSelector
  , setSelectedOfferSelector
  , guestProvidedSpecialRequestTextSelector
  , setGuestProvidedSpecialRequestTextSelector
  , statusSelector
  , setStatusSelector
  , dateStatusModifiedSelector
  , setDateStatusModifiedSelector

  -- * Enum types
  , INRestaurantReservationUserBookingStatus(INRestaurantReservationUserBookingStatus)
  , pattern INRestaurantReservationUserBookingStatusPending
  , pattern INRestaurantReservationUserBookingStatusConfirmed
  , pattern INRestaurantReservationUserBookingStatusDenied

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

-- | @- initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModified :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurant restaurant, IsNSDate bookingDate, IsNSString bookingIdentifier, IsINRestaurantGuest guest, IsNSDate dateStatusModified) => inRestaurantReservationUserBooking -> restaurant -> bookingDate -> CULong -> bookingIdentifier -> guest -> INRestaurantReservationUserBookingStatus -> dateStatusModified -> IO (Id INRestaurantReservationUserBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModified inRestaurantReservationUserBooking  restaurant bookingDate partySize bookingIdentifier guest status dateStatusModified =
withObjCPtr restaurant $ \raw_restaurant ->
  withObjCPtr bookingDate $ \raw_bookingDate ->
    withObjCPtr bookingIdentifier $ \raw_bookingIdentifier ->
      withObjCPtr guest $ \raw_guest ->
        withObjCPtr dateStatusModified $ \raw_dateStatusModified ->
            sendMsg inRestaurantReservationUserBooking (mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ()), argPtr (castPtr raw_bookingDate :: Ptr ()), argCULong (fromIntegral partySize), argPtr (castPtr raw_bookingIdentifier :: Ptr ()), argPtr (castPtr raw_guest :: Ptr ()), argCULong (coerce status), argPtr (castPtr raw_dateStatusModified :: Ptr ())] >>= ownedObject . castPtr

-- | @- guest@
guest :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id INRestaurantGuest)
guest inRestaurantReservationUserBooking  =
  sendMsg inRestaurantReservationUserBooking (mkSelector "guest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuest:@
setGuest :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurantGuest value) => inRestaurantReservationUserBooking -> value -> IO ()
setGuest inRestaurantReservationUserBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationUserBooking (mkSelector "setGuest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- advisementText@
advisementText :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSString)
advisementText inRestaurantReservationUserBooking  =
  sendMsg inRestaurantReservationUserBooking (mkSelector "advisementText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdvisementText:@
setAdvisementText :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSString value) => inRestaurantReservationUserBooking -> value -> IO ()
setAdvisementText inRestaurantReservationUserBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationUserBooking (mkSelector "setAdvisementText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedOffer@
selectedOffer :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id INRestaurantOffer)
selectedOffer inRestaurantReservationUserBooking  =
  sendMsg inRestaurantReservationUserBooking (mkSelector "selectedOffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedOffer:@
setSelectedOffer :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsINRestaurantOffer value) => inRestaurantReservationUserBooking -> value -> IO ()
setSelectedOffer inRestaurantReservationUserBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationUserBooking (mkSelector "setSelectedOffer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- guestProvidedSpecialRequestText@
guestProvidedSpecialRequestText :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSString)
guestProvidedSpecialRequestText inRestaurantReservationUserBooking  =
  sendMsg inRestaurantReservationUserBooking (mkSelector "guestProvidedSpecialRequestText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestText :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSString value) => inRestaurantReservationUserBooking -> value -> IO ()
setGuestProvidedSpecialRequestText inRestaurantReservationUserBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationUserBooking (mkSelector "setGuestProvidedSpecialRequestText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO INRestaurantReservationUserBookingStatus
status inRestaurantReservationUserBooking  =
  fmap (coerce :: CULong -> INRestaurantReservationUserBookingStatus) $ sendMsg inRestaurantReservationUserBooking (mkSelector "status") retCULong []

-- | @- setStatus:@
setStatus :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> INRestaurantReservationUserBookingStatus -> IO ()
setStatus inRestaurantReservationUserBooking  value =
  sendMsg inRestaurantReservationUserBooking (mkSelector "setStatus:") retVoid [argCULong (coerce value)]

-- | @- dateStatusModified@
dateStatusModified :: IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking => inRestaurantReservationUserBooking -> IO (Id NSDate)
dateStatusModified inRestaurantReservationUserBooking  =
  sendMsg inRestaurantReservationUserBooking (mkSelector "dateStatusModified") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateStatusModified:@
setDateStatusModified :: (IsINRestaurantReservationUserBooking inRestaurantReservationUserBooking, IsNSDate value) => inRestaurantReservationUserBooking -> value -> IO ()
setDateStatusModified inRestaurantReservationUserBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationUserBooking (mkSelector "setDateStatusModified:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector :: Selector
initWithRestaurant_bookingDate_partySize_bookingIdentifier_guest_status_dateStatusModifiedSelector = mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:guest:status:dateStatusModified:"

-- | @Selector@ for @guest@
guestSelector :: Selector
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector
setGuestSelector = mkSelector "setGuest:"

-- | @Selector@ for @advisementText@
advisementTextSelector :: Selector
advisementTextSelector = mkSelector "advisementText"

-- | @Selector@ for @setAdvisementText:@
setAdvisementTextSelector :: Selector
setAdvisementTextSelector = mkSelector "setAdvisementText:"

-- | @Selector@ for @selectedOffer@
selectedOfferSelector :: Selector
selectedOfferSelector = mkSelector "selectedOffer"

-- | @Selector@ for @setSelectedOffer:@
setSelectedOfferSelector :: Selector
setSelectedOfferSelector = mkSelector "setSelectedOffer:"

-- | @Selector@ for @guestProvidedSpecialRequestText@
guestProvidedSpecialRequestTextSelector :: Selector
guestProvidedSpecialRequestTextSelector = mkSelector "guestProvidedSpecialRequestText"

-- | @Selector@ for @setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestTextSelector :: Selector
setGuestProvidedSpecialRequestTextSelector = mkSelector "setGuestProvidedSpecialRequestText:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @dateStatusModified@
dateStatusModifiedSelector :: Selector
dateStatusModifiedSelector = mkSelector "dateStatusModified"

-- | @Selector@ for @setDateStatusModified:@
setDateStatusModifiedSelector :: Selector
setDateStatusModifiedSelector = mkSelector "setDateStatusModified:"


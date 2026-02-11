{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBookRestaurantReservationIntent@.
module ObjC.Intents.INBookRestaurantReservationIntent
  ( INBookRestaurantReservationIntent
  , IsINBookRestaurantReservationIntent(..)
  , initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestText
  , restaurant
  , setRestaurant
  , bookingDateComponents
  , setBookingDateComponents
  , partySize
  , setPartySize
  , bookingIdentifier
  , setBookingIdentifier
  , guest
  , setGuest
  , selectedOffer
  , setSelectedOffer
  , guestProvidedSpecialRequestText
  , setGuestProvidedSpecialRequestText
  , initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector
  , restaurantSelector
  , setRestaurantSelector
  , bookingDateComponentsSelector
  , setBookingDateComponentsSelector
  , partySizeSelector
  , setPartySizeSelector
  , bookingIdentifierSelector
  , setBookingIdentifierSelector
  , guestSelector
  , setGuestSelector
  , selectedOfferSelector
  , setSelectedOfferSelector
  , guestProvidedSpecialRequestTextSelector
  , setGuestProvidedSpecialRequestTextSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:@
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestText :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurant restaurant, IsNSDateComponents bookingDateComponents, IsNSString bookingIdentifier, IsINRestaurantGuest guest, IsINRestaurantOffer selectedOffer, IsNSString guestProvidedSpecialRequestText) => inBookRestaurantReservationIntent -> restaurant -> bookingDateComponents -> CULong -> bookingIdentifier -> guest -> selectedOffer -> guestProvidedSpecialRequestText -> IO (Id INBookRestaurantReservationIntent)
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestText inBookRestaurantReservationIntent  restaurant bookingDateComponents partySize bookingIdentifier guest selectedOffer guestProvidedSpecialRequestText =
withObjCPtr restaurant $ \raw_restaurant ->
  withObjCPtr bookingDateComponents $ \raw_bookingDateComponents ->
    withObjCPtr bookingIdentifier $ \raw_bookingIdentifier ->
      withObjCPtr guest $ \raw_guest ->
        withObjCPtr selectedOffer $ \raw_selectedOffer ->
          withObjCPtr guestProvidedSpecialRequestText $ \raw_guestProvidedSpecialRequestText ->
              sendMsg inBookRestaurantReservationIntent (mkSelector "initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ()), argPtr (castPtr raw_bookingDateComponents :: Ptr ()), argCULong (fromIntegral partySize), argPtr (castPtr raw_bookingIdentifier :: Ptr ()), argPtr (castPtr raw_guest :: Ptr ()), argPtr (castPtr raw_selectedOffer :: Ptr ()), argPtr (castPtr raw_guestProvidedSpecialRequestText :: Ptr ())] >>= ownedObject . castPtr

-- | @- restaurant@
restaurant :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurant)
restaurant inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "restaurant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurant:@
setRestaurant :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurant value) => inBookRestaurantReservationIntent -> value -> IO ()
setRestaurant inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setRestaurant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bookingDateComponents@
bookingDateComponents :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSDateComponents)
bookingDateComponents inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "bookingDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBookingDateComponents:@
setBookingDateComponents :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSDateComponents value) => inBookRestaurantReservationIntent -> value -> IO ()
setBookingDateComponents inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setBookingDateComponents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- partySize@
partySize :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO CULong
partySize inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "partySize") retCULong []

-- | @- setPartySize:@
setPartySize :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> CULong -> IO ()
setPartySize inBookRestaurantReservationIntent  value =
  sendMsg inBookRestaurantReservationIntent (mkSelector "setPartySize:") retVoid [argCULong (fromIntegral value)]

-- | @- bookingIdentifier@
bookingIdentifier :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSString)
bookingIdentifier inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "bookingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBookingIdentifier:@
setBookingIdentifier :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSString value) => inBookRestaurantReservationIntent -> value -> IO ()
setBookingIdentifier inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setBookingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- guest@
guest :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurantGuest)
guest inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "guest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuest:@
setGuest :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurantGuest value) => inBookRestaurantReservationIntent -> value -> IO ()
setGuest inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setGuest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedOffer@
selectedOffer :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurantOffer)
selectedOffer inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "selectedOffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedOffer:@
setSelectedOffer :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurantOffer value) => inBookRestaurantReservationIntent -> value -> IO ()
setSelectedOffer inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setSelectedOffer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- guestProvidedSpecialRequestText@
guestProvidedSpecialRequestText :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSString)
guestProvidedSpecialRequestText inBookRestaurantReservationIntent  =
  sendMsg inBookRestaurantReservationIntent (mkSelector "guestProvidedSpecialRequestText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestText :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSString value) => inBookRestaurantReservationIntent -> value -> IO ()
setGuestProvidedSpecialRequestText inBookRestaurantReservationIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inBookRestaurantReservationIntent (mkSelector "setGuestProvidedSpecialRequestText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:@
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector :: Selector
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector = mkSelector "initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @bookingDateComponents@
bookingDateComponentsSelector :: Selector
bookingDateComponentsSelector = mkSelector "bookingDateComponents"

-- | @Selector@ for @setBookingDateComponents:@
setBookingDateComponentsSelector :: Selector
setBookingDateComponentsSelector = mkSelector "setBookingDateComponents:"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @setPartySize:@
setPartySizeSelector :: Selector
setPartySizeSelector = mkSelector "setPartySize:"

-- | @Selector@ for @bookingIdentifier@
bookingIdentifierSelector :: Selector
bookingIdentifierSelector = mkSelector "bookingIdentifier"

-- | @Selector@ for @setBookingIdentifier:@
setBookingIdentifierSelector :: Selector
setBookingIdentifierSelector = mkSelector "setBookingIdentifier:"

-- | @Selector@ for @guest@
guestSelector :: Selector
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector
setGuestSelector = mkSelector "setGuest:"

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


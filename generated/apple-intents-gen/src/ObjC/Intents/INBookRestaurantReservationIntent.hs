{-# LANGUAGE DataKinds #-}
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
  , bookingDateComponentsSelector
  , bookingIdentifierSelector
  , guestProvidedSpecialRequestTextSelector
  , guestSelector
  , initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector
  , partySizeSelector
  , restaurantSelector
  , selectedOfferSelector
  , setBookingDateComponentsSelector
  , setBookingIdentifierSelector
  , setGuestProvidedSpecialRequestTextSelector
  , setGuestSelector
  , setPartySizeSelector
  , setRestaurantSelector
  , setSelectedOfferSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:@
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestText :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurant restaurant, IsNSDateComponents bookingDateComponents, IsNSString bookingIdentifier, IsINRestaurantGuest guest, IsINRestaurantOffer selectedOffer, IsNSString guestProvidedSpecialRequestText) => inBookRestaurantReservationIntent -> restaurant -> bookingDateComponents -> CULong -> bookingIdentifier -> guest -> selectedOffer -> guestProvidedSpecialRequestText -> IO (Id INBookRestaurantReservationIntent)
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestText inBookRestaurantReservationIntent restaurant bookingDateComponents partySize bookingIdentifier guest selectedOffer guestProvidedSpecialRequestText =
  sendOwnedMessage inBookRestaurantReservationIntent initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector (toINRestaurant restaurant) (toNSDateComponents bookingDateComponents) partySize (toNSString bookingIdentifier) (toINRestaurantGuest guest) (toINRestaurantOffer selectedOffer) (toNSString guestProvidedSpecialRequestText)

-- | @- restaurant@
restaurant :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurant)
restaurant inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent restaurantSelector

-- | @- setRestaurant:@
setRestaurant :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurant value) => inBookRestaurantReservationIntent -> value -> IO ()
setRestaurant inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setRestaurantSelector (toINRestaurant value)

-- | @- bookingDateComponents@
bookingDateComponents :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSDateComponents)
bookingDateComponents inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent bookingDateComponentsSelector

-- | @- setBookingDateComponents:@
setBookingDateComponents :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSDateComponents value) => inBookRestaurantReservationIntent -> value -> IO ()
setBookingDateComponents inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setBookingDateComponentsSelector (toNSDateComponents value)

-- | @- partySize@
partySize :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO CULong
partySize inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent partySizeSelector

-- | @- setPartySize:@
setPartySize :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> CULong -> IO ()
setPartySize inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setPartySizeSelector value

-- | @- bookingIdentifier@
bookingIdentifier :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSString)
bookingIdentifier inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent bookingIdentifierSelector

-- | @- setBookingIdentifier:@
setBookingIdentifier :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSString value) => inBookRestaurantReservationIntent -> value -> IO ()
setBookingIdentifier inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setBookingIdentifierSelector (toNSString value)

-- | @- guest@
guest :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurantGuest)
guest inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent guestSelector

-- | @- setGuest:@
setGuest :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurantGuest value) => inBookRestaurantReservationIntent -> value -> IO ()
setGuest inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setGuestSelector (toINRestaurantGuest value)

-- | @- selectedOffer@
selectedOffer :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id INRestaurantOffer)
selectedOffer inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent selectedOfferSelector

-- | @- setSelectedOffer:@
setSelectedOffer :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsINRestaurantOffer value) => inBookRestaurantReservationIntent -> value -> IO ()
setSelectedOffer inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setSelectedOfferSelector (toINRestaurantOffer value)

-- | @- guestProvidedSpecialRequestText@
guestProvidedSpecialRequestText :: IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent => inBookRestaurantReservationIntent -> IO (Id NSString)
guestProvidedSpecialRequestText inBookRestaurantReservationIntent =
  sendMessage inBookRestaurantReservationIntent guestProvidedSpecialRequestTextSelector

-- | @- setGuestProvidedSpecialRequestText:@
setGuestProvidedSpecialRequestText :: (IsINBookRestaurantReservationIntent inBookRestaurantReservationIntent, IsNSString value) => inBookRestaurantReservationIntent -> value -> IO ()
setGuestProvidedSpecialRequestText inBookRestaurantReservationIntent value =
  sendMessage inBookRestaurantReservationIntent setGuestProvidedSpecialRequestTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:@
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector :: Selector '[Id INRestaurant, Id NSDateComponents, CULong, Id NSString, Id INRestaurantGuest, Id INRestaurantOffer, Id NSString] (Id INBookRestaurantReservationIntent)
initWithRestaurant_bookingDateComponents_partySize_bookingIdentifier_guest_selectedOffer_guestProvidedSpecialRequestTextSelector = mkSelector "initWithRestaurant:bookingDateComponents:partySize:bookingIdentifier:guest:selectedOffer:guestProvidedSpecialRequestText:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector '[] (Id INRestaurant)
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector '[Id INRestaurant] ()
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @bookingDateComponents@
bookingDateComponentsSelector :: Selector '[] (Id NSDateComponents)
bookingDateComponentsSelector = mkSelector "bookingDateComponents"

-- | @Selector@ for @setBookingDateComponents:@
setBookingDateComponentsSelector :: Selector '[Id NSDateComponents] ()
setBookingDateComponentsSelector = mkSelector "setBookingDateComponents:"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector '[] CULong
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @setPartySize:@
setPartySizeSelector :: Selector '[CULong] ()
setPartySizeSelector = mkSelector "setPartySize:"

-- | @Selector@ for @bookingIdentifier@
bookingIdentifierSelector :: Selector '[] (Id NSString)
bookingIdentifierSelector = mkSelector "bookingIdentifier"

-- | @Selector@ for @setBookingIdentifier:@
setBookingIdentifierSelector :: Selector '[Id NSString] ()
setBookingIdentifierSelector = mkSelector "setBookingIdentifier:"

-- | @Selector@ for @guest@
guestSelector :: Selector '[] (Id INRestaurantGuest)
guestSelector = mkSelector "guest"

-- | @Selector@ for @setGuest:@
setGuestSelector :: Selector '[Id INRestaurantGuest] ()
setGuestSelector = mkSelector "setGuest:"

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


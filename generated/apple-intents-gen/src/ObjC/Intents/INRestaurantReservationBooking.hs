{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantReservationBooking@.
module ObjC.Intents.INRestaurantReservationBooking
  ( INRestaurantReservationBooking
  , IsINRestaurantReservationBooking(..)
  , initWithRestaurant_bookingDate_partySize_bookingIdentifier
  , restaurant
  , setRestaurant
  , bookingDescription
  , setBookingDescription
  , bookingDate
  , setBookingDate
  , partySize
  , setPartySize
  , bookingIdentifier
  , setBookingIdentifier
  , bookingAvailable
  , setBookingAvailable
  , offers
  , setOffers
  , requiresManualRequest
  , setRequiresManualRequest
  , requiresEmailAddress
  , setRequiresEmailAddress
  , requiresName
  , setRequiresName
  , requiresPhoneNumber
  , setRequiresPhoneNumber
  , bookingAvailableSelector
  , bookingDateSelector
  , bookingDescriptionSelector
  , bookingIdentifierSelector
  , initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector
  , offersSelector
  , partySizeSelector
  , requiresEmailAddressSelector
  , requiresManualRequestSelector
  , requiresNameSelector
  , requiresPhoneNumberSelector
  , restaurantSelector
  , setBookingAvailableSelector
  , setBookingDateSelector
  , setBookingDescriptionSelector
  , setBookingIdentifierSelector
  , setOffersSelector
  , setPartySizeSelector
  , setRequiresEmailAddressSelector
  , setRequiresManualRequestSelector
  , setRequiresNameSelector
  , setRequiresPhoneNumberSelector
  , setRestaurantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRestaurant:bookingDate:partySize:bookingIdentifier:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsINRestaurant restaurant, IsNSDate bookingDate, IsNSString bookingIdentifier) => inRestaurantReservationBooking -> restaurant -> bookingDate -> CULong -> bookingIdentifier -> IO (Id INRestaurantReservationBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifier inRestaurantReservationBooking restaurant bookingDate partySize bookingIdentifier =
  sendOwnedMessage inRestaurantReservationBooking initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector (toINRestaurant restaurant) (toNSDate bookingDate) partySize (toNSString bookingIdentifier)

-- | @- restaurant@
restaurant :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id INRestaurant)
restaurant inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking restaurantSelector

-- | @- setRestaurant:@
setRestaurant :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsINRestaurant value) => inRestaurantReservationBooking -> value -> IO ()
setRestaurant inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setRestaurantSelector (toINRestaurant value)

-- | @- bookingDescription@
bookingDescription :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSString)
bookingDescription inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking bookingDescriptionSelector

-- | @- setBookingDescription:@
setBookingDescription :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSString value) => inRestaurantReservationBooking -> value -> IO ()
setBookingDescription inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setBookingDescriptionSelector (toNSString value)

-- | @- bookingDate@
bookingDate :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSDate)
bookingDate inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking bookingDateSelector

-- | @- setBookingDate:@
setBookingDate :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSDate value) => inRestaurantReservationBooking -> value -> IO ()
setBookingDate inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setBookingDateSelector (toNSDate value)

-- | @- partySize@
partySize :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO CULong
partySize inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking partySizeSelector

-- | @- setPartySize:@
setPartySize :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> CULong -> IO ()
setPartySize inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setPartySizeSelector value

-- | @- bookingIdentifier@
bookingIdentifier :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSString)
bookingIdentifier inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking bookingIdentifierSelector

-- | @- setBookingIdentifier:@
setBookingIdentifier :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSString value) => inRestaurantReservationBooking -> value -> IO ()
setBookingIdentifier inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setBookingIdentifierSelector (toNSString value)

-- | @- bookingAvailable@
bookingAvailable :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
bookingAvailable inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking bookingAvailableSelector

-- | @- setBookingAvailable:@
setBookingAvailable :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setBookingAvailable inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setBookingAvailableSelector value

-- | @- offers@
offers :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSArray)
offers inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking offersSelector

-- | @- setOffers:@
setOffers :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSArray value) => inRestaurantReservationBooking -> value -> IO ()
setOffers inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setOffersSelector (toNSArray value)

-- | @- requiresManualRequest@
requiresManualRequest :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresManualRequest inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking requiresManualRequestSelector

-- | @- setRequiresManualRequest:@
setRequiresManualRequest :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresManualRequest inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setRequiresManualRequestSelector value

-- | @- requiresEmailAddress@
requiresEmailAddress :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresEmailAddress inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking requiresEmailAddressSelector

-- | @- setRequiresEmailAddress:@
setRequiresEmailAddress :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresEmailAddress inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setRequiresEmailAddressSelector value

-- | @- requiresName@
requiresName :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresName inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking requiresNameSelector

-- | @- setRequiresName:@
setRequiresName :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresName inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setRequiresNameSelector value

-- | @- requiresPhoneNumber@
requiresPhoneNumber :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresPhoneNumber inRestaurantReservationBooking =
  sendMessage inRestaurantReservationBooking requiresPhoneNumberSelector

-- | @- setRequiresPhoneNumber:@
setRequiresPhoneNumber :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresPhoneNumber inRestaurantReservationBooking value =
  sendMessage inRestaurantReservationBooking setRequiresPhoneNumberSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDate:partySize:bookingIdentifier:@
initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector :: Selector '[Id INRestaurant, Id NSDate, CULong, Id NSString] (Id INRestaurantReservationBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector = mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector '[] (Id INRestaurant)
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector '[Id INRestaurant] ()
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @bookingDescription@
bookingDescriptionSelector :: Selector '[] (Id NSString)
bookingDescriptionSelector = mkSelector "bookingDescription"

-- | @Selector@ for @setBookingDescription:@
setBookingDescriptionSelector :: Selector '[Id NSString] ()
setBookingDescriptionSelector = mkSelector "setBookingDescription:"

-- | @Selector@ for @bookingDate@
bookingDateSelector :: Selector '[] (Id NSDate)
bookingDateSelector = mkSelector "bookingDate"

-- | @Selector@ for @setBookingDate:@
setBookingDateSelector :: Selector '[Id NSDate] ()
setBookingDateSelector = mkSelector "setBookingDate:"

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

-- | @Selector@ for @bookingAvailable@
bookingAvailableSelector :: Selector '[] Bool
bookingAvailableSelector = mkSelector "bookingAvailable"

-- | @Selector@ for @setBookingAvailable:@
setBookingAvailableSelector :: Selector '[Bool] ()
setBookingAvailableSelector = mkSelector "setBookingAvailable:"

-- | @Selector@ for @offers@
offersSelector :: Selector '[] (Id NSArray)
offersSelector = mkSelector "offers"

-- | @Selector@ for @setOffers:@
setOffersSelector :: Selector '[Id NSArray] ()
setOffersSelector = mkSelector "setOffers:"

-- | @Selector@ for @requiresManualRequest@
requiresManualRequestSelector :: Selector '[] Bool
requiresManualRequestSelector = mkSelector "requiresManualRequest"

-- | @Selector@ for @setRequiresManualRequest:@
setRequiresManualRequestSelector :: Selector '[Bool] ()
setRequiresManualRequestSelector = mkSelector "setRequiresManualRequest:"

-- | @Selector@ for @requiresEmailAddress@
requiresEmailAddressSelector :: Selector '[] Bool
requiresEmailAddressSelector = mkSelector "requiresEmailAddress"

-- | @Selector@ for @setRequiresEmailAddress:@
setRequiresEmailAddressSelector :: Selector '[Bool] ()
setRequiresEmailAddressSelector = mkSelector "setRequiresEmailAddress:"

-- | @Selector@ for @requiresName@
requiresNameSelector :: Selector '[] Bool
requiresNameSelector = mkSelector "requiresName"

-- | @Selector@ for @setRequiresName:@
setRequiresNameSelector :: Selector '[Bool] ()
setRequiresNameSelector = mkSelector "setRequiresName:"

-- | @Selector@ for @requiresPhoneNumber@
requiresPhoneNumberSelector :: Selector '[] Bool
requiresPhoneNumberSelector = mkSelector "requiresPhoneNumber"

-- | @Selector@ for @setRequiresPhoneNumber:@
setRequiresPhoneNumberSelector :: Selector '[Bool] ()
setRequiresPhoneNumberSelector = mkSelector "setRequiresPhoneNumber:"


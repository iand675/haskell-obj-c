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
  , initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector
  , restaurantSelector
  , setRestaurantSelector
  , bookingDescriptionSelector
  , setBookingDescriptionSelector
  , bookingDateSelector
  , setBookingDateSelector
  , partySizeSelector
  , setPartySizeSelector
  , bookingIdentifierSelector
  , setBookingIdentifierSelector
  , bookingAvailableSelector
  , setBookingAvailableSelector
  , offersSelector
  , setOffersSelector
  , requiresManualRequestSelector
  , setRequiresManualRequestSelector
  , requiresEmailAddressSelector
  , setRequiresEmailAddressSelector
  , requiresNameSelector
  , setRequiresNameSelector
  , requiresPhoneNumberSelector
  , setRequiresPhoneNumberSelector


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

-- | @- initWithRestaurant:bookingDate:partySize:bookingIdentifier:@
initWithRestaurant_bookingDate_partySize_bookingIdentifier :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsINRestaurant restaurant, IsNSDate bookingDate, IsNSString bookingIdentifier) => inRestaurantReservationBooking -> restaurant -> bookingDate -> CULong -> bookingIdentifier -> IO (Id INRestaurantReservationBooking)
initWithRestaurant_bookingDate_partySize_bookingIdentifier inRestaurantReservationBooking  restaurant bookingDate partySize bookingIdentifier =
withObjCPtr restaurant $ \raw_restaurant ->
  withObjCPtr bookingDate $ \raw_bookingDate ->
    withObjCPtr bookingIdentifier $ \raw_bookingIdentifier ->
        sendMsg inRestaurantReservationBooking (mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_restaurant :: Ptr ()), argPtr (castPtr raw_bookingDate :: Ptr ()), argCULong (fromIntegral partySize), argPtr (castPtr raw_bookingIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- restaurant@
restaurant :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id INRestaurant)
restaurant inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "restaurant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestaurant:@
setRestaurant :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsINRestaurant value) => inRestaurantReservationBooking -> value -> IO ()
setRestaurant inRestaurantReservationBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationBooking (mkSelector "setRestaurant:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bookingDescription@
bookingDescription :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSString)
bookingDescription inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "bookingDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBookingDescription:@
setBookingDescription :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSString value) => inRestaurantReservationBooking -> value -> IO ()
setBookingDescription inRestaurantReservationBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationBooking (mkSelector "setBookingDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bookingDate@
bookingDate :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSDate)
bookingDate inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "bookingDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBookingDate:@
setBookingDate :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSDate value) => inRestaurantReservationBooking -> value -> IO ()
setBookingDate inRestaurantReservationBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationBooking (mkSelector "setBookingDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- partySize@
partySize :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO CULong
partySize inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "partySize") retCULong []

-- | @- setPartySize:@
setPartySize :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> CULong -> IO ()
setPartySize inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setPartySize:") retVoid [argCULong (fromIntegral value)]

-- | @- bookingIdentifier@
bookingIdentifier :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSString)
bookingIdentifier inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "bookingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBookingIdentifier:@
setBookingIdentifier :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSString value) => inRestaurantReservationBooking -> value -> IO ()
setBookingIdentifier inRestaurantReservationBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationBooking (mkSelector "setBookingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bookingAvailable@
bookingAvailable :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
bookingAvailable inRestaurantReservationBooking  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantReservationBooking (mkSelector "bookingAvailable") retCULong []

-- | @- setBookingAvailable:@
setBookingAvailable :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setBookingAvailable inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setBookingAvailable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- offers@
offers :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO (Id NSArray)
offers inRestaurantReservationBooking  =
  sendMsg inRestaurantReservationBooking (mkSelector "offers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffers:@
setOffers :: (IsINRestaurantReservationBooking inRestaurantReservationBooking, IsNSArray value) => inRestaurantReservationBooking -> value -> IO ()
setOffers inRestaurantReservationBooking  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantReservationBooking (mkSelector "setOffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiresManualRequest@
requiresManualRequest :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresManualRequest inRestaurantReservationBooking  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantReservationBooking (mkSelector "requiresManualRequest") retCULong []

-- | @- setRequiresManualRequest:@
setRequiresManualRequest :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresManualRequest inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setRequiresManualRequest:") retVoid [argCULong (if value then 1 else 0)]

-- | @- requiresEmailAddress@
requiresEmailAddress :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresEmailAddress inRestaurantReservationBooking  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantReservationBooking (mkSelector "requiresEmailAddress") retCULong []

-- | @- setRequiresEmailAddress:@
setRequiresEmailAddress :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresEmailAddress inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setRequiresEmailAddress:") retVoid [argCULong (if value then 1 else 0)]

-- | @- requiresName@
requiresName :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresName inRestaurantReservationBooking  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantReservationBooking (mkSelector "requiresName") retCULong []

-- | @- setRequiresName:@
setRequiresName :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresName inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setRequiresName:") retVoid [argCULong (if value then 1 else 0)]

-- | @- requiresPhoneNumber@
requiresPhoneNumber :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> IO Bool
requiresPhoneNumber inRestaurantReservationBooking  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantReservationBooking (mkSelector "requiresPhoneNumber") retCULong []

-- | @- setRequiresPhoneNumber:@
setRequiresPhoneNumber :: IsINRestaurantReservationBooking inRestaurantReservationBooking => inRestaurantReservationBooking -> Bool -> IO ()
setRequiresPhoneNumber inRestaurantReservationBooking  value =
  sendMsg inRestaurantReservationBooking (mkSelector "setRequiresPhoneNumber:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRestaurant:bookingDate:partySize:bookingIdentifier:@
initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector :: Selector
initWithRestaurant_bookingDate_partySize_bookingIdentifierSelector = mkSelector "initWithRestaurant:bookingDate:partySize:bookingIdentifier:"

-- | @Selector@ for @restaurant@
restaurantSelector :: Selector
restaurantSelector = mkSelector "restaurant"

-- | @Selector@ for @setRestaurant:@
setRestaurantSelector :: Selector
setRestaurantSelector = mkSelector "setRestaurant:"

-- | @Selector@ for @bookingDescription@
bookingDescriptionSelector :: Selector
bookingDescriptionSelector = mkSelector "bookingDescription"

-- | @Selector@ for @setBookingDescription:@
setBookingDescriptionSelector :: Selector
setBookingDescriptionSelector = mkSelector "setBookingDescription:"

-- | @Selector@ for @bookingDate@
bookingDateSelector :: Selector
bookingDateSelector = mkSelector "bookingDate"

-- | @Selector@ for @setBookingDate:@
setBookingDateSelector :: Selector
setBookingDateSelector = mkSelector "setBookingDate:"

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

-- | @Selector@ for @bookingAvailable@
bookingAvailableSelector :: Selector
bookingAvailableSelector = mkSelector "bookingAvailable"

-- | @Selector@ for @setBookingAvailable:@
setBookingAvailableSelector :: Selector
setBookingAvailableSelector = mkSelector "setBookingAvailable:"

-- | @Selector@ for @offers@
offersSelector :: Selector
offersSelector = mkSelector "offers"

-- | @Selector@ for @setOffers:@
setOffersSelector :: Selector
setOffersSelector = mkSelector "setOffers:"

-- | @Selector@ for @requiresManualRequest@
requiresManualRequestSelector :: Selector
requiresManualRequestSelector = mkSelector "requiresManualRequest"

-- | @Selector@ for @setRequiresManualRequest:@
setRequiresManualRequestSelector :: Selector
setRequiresManualRequestSelector = mkSelector "setRequiresManualRequest:"

-- | @Selector@ for @requiresEmailAddress@
requiresEmailAddressSelector :: Selector
requiresEmailAddressSelector = mkSelector "requiresEmailAddress"

-- | @Selector@ for @setRequiresEmailAddress:@
setRequiresEmailAddressSelector :: Selector
setRequiresEmailAddressSelector = mkSelector "setRequiresEmailAddress:"

-- | @Selector@ for @requiresName@
requiresNameSelector :: Selector
requiresNameSelector = mkSelector "requiresName"

-- | @Selector@ for @setRequiresName:@
setRequiresNameSelector :: Selector
setRequiresNameSelector = mkSelector "setRequiresName:"

-- | @Selector@ for @requiresPhoneNumber@
requiresPhoneNumberSelector :: Selector
requiresPhoneNumberSelector = mkSelector "requiresPhoneNumber"

-- | @Selector@ for @setRequiresPhoneNumber:@
setRequiresPhoneNumberSelector :: Selector
setRequiresPhoneNumberSelector = mkSelector "setRequiresPhoneNumber:"


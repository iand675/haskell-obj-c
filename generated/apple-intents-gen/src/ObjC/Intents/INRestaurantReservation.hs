{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantReservation@.
module ObjC.Intents.INRestaurantReservation
  ( INRestaurantReservation
  , IsINRestaurantReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation
  , reservationDuration
  , partySize
  , restaurantLocation
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector
  , partySizeSelector
  , reservationDurationSelector
  , restaurantLocationSelector

  -- * Enum types
  , INReservationStatus(INReservationStatus)
  , pattern INReservationStatusUnknown
  , pattern INReservationStatusCanceled
  , pattern INReservationStatusPending
  , pattern INReservationStatusHold
  , pattern INReservationStatusConfirmed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation :: (IsINRestaurantReservation inRestaurantReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINDateComponentsRange reservationDuration, IsNSNumber partySize, IsCLPlacemark restaurantLocation) => inRestaurantReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservationDuration -> partySize -> restaurantLocation -> IO (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocation inRestaurantReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservationDuration partySize restaurantLocation =
  sendOwnedMessage inRestaurantReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toINDateComponentsRange reservationDuration) (toNSNumber partySize) (toCLPlacemark restaurantLocation)

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation :: (IsINRestaurantReservation inRestaurantReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINDateComponentsRange reservationDuration, IsNSNumber partySize, IsCLPlacemark restaurantLocation) => inRestaurantReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservationDuration -> partySize -> restaurantLocation -> IO (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocation inRestaurantReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservationDuration partySize restaurantLocation =
  sendOwnedMessage inRestaurantReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toINDateComponentsRange reservationDuration) (toNSNumber partySize) (toCLPlacemark restaurantLocation)

-- | @- reservationDuration@
reservationDuration :: IsINRestaurantReservation inRestaurantReservation => inRestaurantReservation -> IO (Id INDateComponentsRange)
reservationDuration inRestaurantReservation =
  sendMessage inRestaurantReservation reservationDurationSelector

-- | @- partySize@
partySize :: IsINRestaurantReservation inRestaurantReservation => inRestaurantReservation -> IO (Id NSNumber)
partySize inRestaurantReservation =
  sendMessage inRestaurantReservation partySizeSelector

-- | @- restaurantLocation@
restaurantLocation :: IsINRestaurantReservation inRestaurantReservation => inRestaurantReservation -> IO (Id CLPlacemark)
restaurantLocation inRestaurantReservation =
  sendMessage inRestaurantReservation restaurantLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id INDateComponentsRange, Id NSNumber, Id CLPlacemark] (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservationDuration_partySize_restaurantLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservationDuration:partySize:restaurantLocation:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id INDateComponentsRange, Id NSNumber, Id CLPlacemark] (Id INRestaurantReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservationDuration_partySize_restaurantLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservationDuration:partySize:restaurantLocation:"

-- | @Selector@ for @reservationDuration@
reservationDurationSelector :: Selector '[] (Id INDateComponentsRange)
reservationDurationSelector = mkSelector "reservationDuration"

-- | @Selector@ for @partySize@
partySizeSelector :: Selector '[] (Id NSNumber)
partySizeSelector = mkSelector "partySize"

-- | @Selector@ for @restaurantLocation@
restaurantLocationSelector :: Selector '[] (Id CLPlacemark)
restaurantLocationSelector = mkSelector "restaurantLocation"


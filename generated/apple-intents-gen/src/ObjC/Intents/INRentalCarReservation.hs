{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRentalCarReservation@.
module ObjC.Intents.INRentalCarReservation
  ( INRentalCarReservation
  , IsINRentalCarReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocation
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocation
  , rentalCar
  , rentalDuration
  , pickupLocation
  , dropOffLocation
  , dropOffLocationSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector
  , pickupLocationSelector
  , rentalCarSelector
  , rentalDurationSelector

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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocation :: (IsINRentalCarReservation inRentalCarReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINRentalCar rentalCar, IsINDateComponentsRange rentalDuration, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inRentalCarReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> rentalCar -> rentalDuration -> pickupLocation -> dropOffLocation -> IO (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocation inRentalCarReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url rentalCar rentalDuration pickupLocation dropOffLocation =
  sendOwnedMessage inRentalCarReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toINRentalCar rentalCar) (toINDateComponentsRange rentalDuration) (toCLPlacemark pickupLocation) (toCLPlacemark dropOffLocation)

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocation :: (IsINRentalCarReservation inRentalCarReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINRentalCar rentalCar, IsINDateComponentsRange rentalDuration, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inRentalCarReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> rentalCar -> rentalDuration -> pickupLocation -> dropOffLocation -> IO (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocation inRentalCarReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions rentalCar rentalDuration pickupLocation dropOffLocation =
  sendOwnedMessage inRentalCarReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toINRentalCar rentalCar) (toINDateComponentsRange rentalDuration) (toCLPlacemark pickupLocation) (toCLPlacemark dropOffLocation)

-- | @- rentalCar@
rentalCar :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id INRentalCar)
rentalCar inRentalCarReservation =
  sendMessage inRentalCarReservation rentalCarSelector

-- | @- rentalDuration@
rentalDuration :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id INDateComponentsRange)
rentalDuration inRentalCarReservation =
  sendMessage inRentalCarReservation rentalDurationSelector

-- | @- pickupLocation@
pickupLocation :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id CLPlacemark)
pickupLocation inRentalCarReservation =
  sendMessage inRentalCarReservation pickupLocationSelector

-- | @- dropOffLocation@
dropOffLocation :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id CLPlacemark)
dropOffLocation inRentalCarReservation =
  sendMessage inRentalCarReservation dropOffLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id INRentalCar, Id INDateComponentsRange, Id CLPlacemark, Id CLPlacemark] (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id INRentalCar, Id INDateComponentsRange, Id CLPlacemark, Id CLPlacemark] (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:"

-- | @Selector@ for @rentalCar@
rentalCarSelector :: Selector '[] (Id INRentalCar)
rentalCarSelector = mkSelector "rentalCar"

-- | @Selector@ for @rentalDuration@
rentalDurationSelector :: Selector '[] (Id INDateComponentsRange)
rentalDurationSelector = mkSelector "rentalDuration"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector '[] (Id CLPlacemark)
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector '[] (Id CLPlacemark)
dropOffLocationSelector = mkSelector "dropOffLocation"


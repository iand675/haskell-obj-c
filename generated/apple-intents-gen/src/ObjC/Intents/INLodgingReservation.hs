{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLodgingReservation@.
module ObjC.Intents.INLodgingReservation
  ( INLodgingReservation
  , IsINLodgingReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren
  , lodgingBusinessLocation
  , reservationDuration
  , numberOfAdults
  , numberOfChildren
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector
  , lodgingBusinessLocationSelector
  , numberOfAdultsSelector
  , numberOfChildrenSelector
  , reservationDurationSelector

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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren :: (IsINLodgingReservation inLodgingReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsCLPlacemark lodgingBusinessLocation, IsINDateComponentsRange reservationDuration, IsNSNumber numberOfAdults, IsNSNumber numberOfChildren) => inLodgingReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> lodgingBusinessLocation -> reservationDuration -> numberOfAdults -> numberOfChildren -> IO (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren inLodgingReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url lodgingBusinessLocation reservationDuration numberOfAdults numberOfChildren =
  sendOwnedMessage inLodgingReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toNSURL url) (toCLPlacemark lodgingBusinessLocation) (toINDateComponentsRange reservationDuration) (toNSNumber numberOfAdults) (toNSNumber numberOfChildren)

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren :: (IsINLodgingReservation inLodgingReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsCLPlacemark lodgingBusinessLocation, IsINDateComponentsRange reservationDuration, IsNSNumber numberOfAdults, IsNSNumber numberOfChildren) => inLodgingReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> lodgingBusinessLocation -> reservationDuration -> numberOfAdults -> numberOfChildren -> IO (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildren inLodgingReservation itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions lodgingBusinessLocation reservationDuration numberOfAdults numberOfChildren =
  sendOwnedMessage inLodgingReservation initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector (toINSpeakableString itemReference) (toNSString reservationNumber) (toNSDate bookingTime) reservationStatus (toNSString reservationHolderName) (toNSArray actions) (toCLPlacemark lodgingBusinessLocation) (toINDateComponentsRange reservationDuration) (toNSNumber numberOfAdults) (toNSNumber numberOfChildren)

-- | @- lodgingBusinessLocation@
lodgingBusinessLocation :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id CLPlacemark)
lodgingBusinessLocation inLodgingReservation =
  sendMessage inLodgingReservation lodgingBusinessLocationSelector

-- | @- reservationDuration@
reservationDuration :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id INDateComponentsRange)
reservationDuration inLodgingReservation =
  sendMessage inLodgingReservation reservationDurationSelector

-- | @- numberOfAdults@
numberOfAdults :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id NSNumber)
numberOfAdults inLodgingReservation =
  sendMessage inLodgingReservation numberOfAdultsSelector

-- | @- numberOfChildren@
numberOfChildren :: IsINLodgingReservation inLodgingReservation => inLodgingReservation -> IO (Id NSNumber)
numberOfChildren inLodgingReservation =
  sendMessage inLodgingReservation numberOfChildrenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id NSURL, Id CLPlacemark, Id INDateComponentsRange, Id NSNumber, Id NSNumber] (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector :: Selector '[Id INSpeakableString, Id NSString, Id NSDate, INReservationStatus, Id NSString, Id NSArray, Id CLPlacemark, Id INDateComponentsRange, Id NSNumber, Id NSNumber] (Id INLodgingReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_lodgingBusinessLocation_reservationDuration_numberOfAdults_numberOfChildrenSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:lodgingBusinessLocation:reservationDuration:numberOfAdults:numberOfChildren:"

-- | @Selector@ for @lodgingBusinessLocation@
lodgingBusinessLocationSelector :: Selector '[] (Id CLPlacemark)
lodgingBusinessLocationSelector = mkSelector "lodgingBusinessLocation"

-- | @Selector@ for @reservationDuration@
reservationDurationSelector :: Selector '[] (Id INDateComponentsRange)
reservationDurationSelector = mkSelector "reservationDuration"

-- | @Selector@ for @numberOfAdults@
numberOfAdultsSelector :: Selector '[] (Id NSNumber)
numberOfAdultsSelector = mkSelector "numberOfAdults"

-- | @Selector@ for @numberOfChildren@
numberOfChildrenSelector :: Selector '[] (Id NSNumber)
numberOfChildrenSelector = mkSelector "numberOfChildren"


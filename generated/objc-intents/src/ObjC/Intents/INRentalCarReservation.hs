{-# LANGUAGE PatternSynonyms #-}
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
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector
  , rentalCarSelector
  , rentalDurationSelector
  , pickupLocationSelector
  , dropOffLocationSelector

  -- * Enum types
  , INReservationStatus(INReservationStatus)
  , pattern INReservationStatusUnknown
  , pattern INReservationStatusCanceled
  , pattern INReservationStatusPending
  , pattern INReservationStatusHold
  , pattern INReservationStatusConfirmed

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
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocation :: (IsINRentalCarReservation inRentalCarReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINRentalCar rentalCar, IsINDateComponentsRange rentalDuration, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inRentalCarReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> rentalCar -> rentalDuration -> pickupLocation -> dropOffLocation -> IO (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocation inRentalCarReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url rentalCar rentalDuration pickupLocation dropOffLocation =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr rentalCar $ \raw_rentalCar ->
              withObjCPtr rentalDuration $ \raw_rentalDuration ->
                withObjCPtr pickupLocation $ \raw_pickupLocation ->
                  withObjCPtr dropOffLocation $ \raw_dropOffLocation ->
                      sendMsg inRentalCarReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_rentalCar :: Ptr ()), argPtr (castPtr raw_rentalDuration :: Ptr ()), argPtr (castPtr raw_pickupLocation :: Ptr ()), argPtr (castPtr raw_dropOffLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocation :: (IsINRentalCarReservation inRentalCarReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINRentalCar rentalCar, IsINDateComponentsRange rentalDuration, IsCLPlacemark pickupLocation, IsCLPlacemark dropOffLocation) => inRentalCarReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> rentalCar -> rentalDuration -> pickupLocation -> dropOffLocation -> IO (Id INRentalCarReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocation inRentalCarReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions rentalCar rentalDuration pickupLocation dropOffLocation =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr rentalCar $ \raw_rentalCar ->
            withObjCPtr rentalDuration $ \raw_rentalDuration ->
              withObjCPtr pickupLocation $ \raw_pickupLocation ->
                withObjCPtr dropOffLocation $ \raw_dropOffLocation ->
                    sendMsg inRentalCarReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_rentalCar :: Ptr ()), argPtr (castPtr raw_rentalDuration :: Ptr ()), argPtr (castPtr raw_pickupLocation :: Ptr ()), argPtr (castPtr raw_dropOffLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- rentalCar@
rentalCar :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id INRentalCar)
rentalCar inRentalCarReservation  =
  sendMsg inRentalCarReservation (mkSelector "rentalCar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rentalDuration@
rentalDuration :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id INDateComponentsRange)
rentalDuration inRentalCarReservation  =
  sendMsg inRentalCarReservation (mkSelector "rentalDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pickupLocation@
pickupLocation :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id CLPlacemark)
pickupLocation inRentalCarReservation  =
  sendMsg inRentalCarReservation (mkSelector "pickupLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dropOffLocation@
dropOffLocation :: IsINRentalCarReservation inRentalCarReservation => inRentalCarReservation -> IO (Id CLPlacemark)
dropOffLocation inRentalCarReservation  =
  sendMsg inRentalCarReservation (mkSelector "dropOffLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:rentalCar:rentalDuration:pickupLocation:dropOffLocation:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_rentalCar_rentalDuration_pickupLocation_dropOffLocationSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:rentalCar:rentalDuration:pickupLocation:dropOffLocation:"

-- | @Selector@ for @rentalCar@
rentalCarSelector :: Selector
rentalCarSelector = mkSelector "rentalCar"

-- | @Selector@ for @rentalDuration@
rentalDurationSelector :: Selector
rentalDurationSelector = mkSelector "rentalDuration"

-- | @Selector@ for @pickupLocation@
pickupLocationSelector :: Selector
pickupLocationSelector = mkSelector "pickupLocation"

-- | @Selector@ for @dropOffLocation@
dropOffLocationSelector :: Selector
dropOffLocationSelector = mkSelector "dropOffLocation"


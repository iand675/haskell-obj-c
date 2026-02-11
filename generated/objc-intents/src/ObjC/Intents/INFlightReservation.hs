{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFlightReservation@.
module ObjC.Intents.INFlightReservation
  ( INFlightReservation
  , IsINFlightReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight
  , reservedSeat
  , flight
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector
  , reservedSeatSelector
  , flightSelector

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
import ObjC.Foundation.Internal.Classes

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight :: (IsINFlightReservation inFlightReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINFlight flight) => inFlightReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> flight -> IO (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flight inFlightReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat flight =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservedSeat $ \raw_reservedSeat ->
              withObjCPtr flight $ \raw_flight ->
                  sendMsg inFlightReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_flight :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight :: (IsINFlightReservation inFlightReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINSeat reservedSeat, IsINFlight flight) => inFlightReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservedSeat -> flight -> IO (Id INFlightReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flight inFlightReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservedSeat flight =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr reservedSeat $ \raw_reservedSeat ->
            withObjCPtr flight $ \raw_flight ->
                sendMsg inFlightReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_flight :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservedSeat@
reservedSeat :: IsINFlightReservation inFlightReservation => inFlightReservation -> IO (Id INSeat)
reservedSeat inFlightReservation  =
  sendMsg inFlightReservation (mkSelector "reservedSeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- flight@
flight :: IsINFlightReservation inFlightReservation => inFlightReservation -> IO (Id INFlight)
flight inFlightReservation  =
  sendMsg inFlightReservation (mkSelector "flight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_flightSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:flight:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_flightSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:flight:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @flight@
flightSelector :: Selector
flightSelector = mkSelector "flight"


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBoatReservation@.
module ObjC.Intents.INBoatReservation
  ( INBoatReservation
  , IsINBoatReservation(..)
  , init_
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip
  , reservedSeat
  , boatTrip
  , initSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector
  , reservedSeatSelector
  , boatTripSelector

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

-- | @- init@
init_ :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INBoatReservation)
init_ inBoatReservation  =
  sendMsg inBoatReservation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip :: (IsINBoatReservation inBoatReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINBoatTrip boatTrip) => inBoatReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> boatTrip -> IO (Id INBoatReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTrip inBoatReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat boatTrip =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservedSeat $ \raw_reservedSeat ->
              withObjCPtr boatTrip $ \raw_boatTrip ->
                  sendMsg inBoatReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_boatTrip :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservedSeat@
reservedSeat :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INSeat)
reservedSeat inBoatReservation  =
  sendMsg inBoatReservation (mkSelector "reservedSeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boatTrip@
boatTrip :: IsINBoatReservation inBoatReservation => inBoatReservation -> IO (Id INBoatTrip)
boatTrip inBoatReservation  =
  sendMsg inBoatReservation (mkSelector "boatTrip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_boatTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:boatTrip:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @boatTrip@
boatTripSelector :: Selector
boatTripSelector = mkSelector "boatTrip"


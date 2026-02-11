{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTrainReservation@.
module ObjC.Intents.INTrainReservation
  ( INTrainReservation
  , IsINTrainReservation(..)
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTrip
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTrip
  , reservedSeat
  , trainTrip
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTripSelector
  , initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTripSelector
  , reservedSeatSelector
  , trainTripSelector

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

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:trainTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTrip :: (IsINTrainReservation inTrainReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsNSURL url, IsINSeat reservedSeat, IsINTrainTrip trainTrip) => inTrainReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> url -> reservedSeat -> trainTrip -> IO (Id INTrainReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTrip inTrainReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions url reservedSeat trainTrip =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr url $ \raw_url ->
            withObjCPtr reservedSeat $ \raw_reservedSeat ->
              withObjCPtr trainTrip $ \raw_trainTrip ->
                  sendMsg inTrainReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:trainTrip:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_trainTrip :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:trainTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTrip :: (IsINTrainReservation inTrainReservation, IsINSpeakableString itemReference, IsNSString reservationNumber, IsNSDate bookingTime, IsNSString reservationHolderName, IsNSArray actions, IsINSeat reservedSeat, IsINTrainTrip trainTrip) => inTrainReservation -> itemReference -> reservationNumber -> bookingTime -> INReservationStatus -> reservationHolderName -> actions -> reservedSeat -> trainTrip -> IO (Id INTrainReservation)
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTrip inTrainReservation  itemReference reservationNumber bookingTime reservationStatus reservationHolderName actions reservedSeat trainTrip =
withObjCPtr itemReference $ \raw_itemReference ->
  withObjCPtr reservationNumber $ \raw_reservationNumber ->
    withObjCPtr bookingTime $ \raw_bookingTime ->
      withObjCPtr reservationHolderName $ \raw_reservationHolderName ->
        withObjCPtr actions $ \raw_actions ->
          withObjCPtr reservedSeat $ \raw_reservedSeat ->
            withObjCPtr trainTrip $ \raw_trainTrip ->
                sendMsg inTrainReservation (mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:trainTrip:") (retPtr retVoid) [argPtr (castPtr raw_itemReference :: Ptr ()), argPtr (castPtr raw_reservationNumber :: Ptr ()), argPtr (castPtr raw_bookingTime :: Ptr ()), argCLong (coerce reservationStatus), argPtr (castPtr raw_reservationHolderName :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_reservedSeat :: Ptr ()), argPtr (castPtr raw_trainTrip :: Ptr ())] >>= ownedObject . castPtr

-- | @- reservedSeat@
reservedSeat :: IsINTrainReservation inTrainReservation => inTrainReservation -> IO (Id INSeat)
reservedSeat inTrainReservation  =
  sendMsg inTrainReservation (mkSelector "reservedSeat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trainTrip@
trainTrip :: IsINTrainReservation inTrainReservation => inTrainReservation -> IO (Id INTrainTrip)
trainTrip inTrainReservation  =
  sendMsg inTrainReservation (mkSelector "trainTrip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:trainTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTripSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_URL_reservedSeat_trainTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:URL:reservedSeat:trainTrip:"

-- | @Selector@ for @initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:trainTrip:@
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTripSelector :: Selector
initWithItemReference_reservationNumber_bookingTime_reservationStatus_reservationHolderName_actions_reservedSeat_trainTripSelector = mkSelector "initWithItemReference:reservationNumber:bookingTime:reservationStatus:reservationHolderName:actions:reservedSeat:trainTrip:"

-- | @Selector@ for @reservedSeat@
reservedSeatSelector :: Selector
reservedSeatSelector = mkSelector "reservedSeat"

-- | @Selector@ for @trainTrip@
trainTripSelector :: Selector
trainTripSelector = mkSelector "trainTrip"


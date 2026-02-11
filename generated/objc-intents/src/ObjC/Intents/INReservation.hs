{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INReservation@.
module ObjC.Intents.INReservation
  ( INReservation
  , IsINReservation(..)
  , init_
  , itemReference
  , reservationNumber
  , bookingTime
  , reservationStatus
  , reservationHolderName
  , actions
  , url
  , initSelector
  , itemReferenceSelector
  , reservationNumberSelector
  , bookingTimeSelector
  , reservationStatusSelector
  , reservationHolderNameSelector
  , actionsSelector
  , urlSelector

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
init_ :: IsINReservation inReservation => inReservation -> IO (Id INReservation)
init_ inReservation  =
  sendMsg inReservation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- itemReference@
itemReference :: IsINReservation inReservation => inReservation -> IO (Id INSpeakableString)
itemReference inReservation  =
  sendMsg inReservation (mkSelector "itemReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reservationNumber@
reservationNumber :: IsINReservation inReservation => inReservation -> IO (Id NSString)
reservationNumber inReservation  =
  sendMsg inReservation (mkSelector "reservationNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bookingTime@
bookingTime :: IsINReservation inReservation => inReservation -> IO (Id NSDate)
bookingTime inReservation  =
  sendMsg inReservation (mkSelector "bookingTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reservationStatus@
reservationStatus :: IsINReservation inReservation => inReservation -> IO INReservationStatus
reservationStatus inReservation  =
  fmap (coerce :: CLong -> INReservationStatus) $ sendMsg inReservation (mkSelector "reservationStatus") retCLong []

-- | @- reservationHolderName@
reservationHolderName :: IsINReservation inReservation => inReservation -> IO (Id NSString)
reservationHolderName inReservation  =
  sendMsg inReservation (mkSelector "reservationHolderName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- actions@
actions :: IsINReservation inReservation => inReservation -> IO (Id NSArray)
actions inReservation  =
  sendMsg inReservation (mkSelector "actions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsINReservation inReservation => inReservation -> IO (Id NSURL)
url inReservation  =
  sendMsg inReservation (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @itemReference@
itemReferenceSelector :: Selector
itemReferenceSelector = mkSelector "itemReference"

-- | @Selector@ for @reservationNumber@
reservationNumberSelector :: Selector
reservationNumberSelector = mkSelector "reservationNumber"

-- | @Selector@ for @bookingTime@
bookingTimeSelector :: Selector
bookingTimeSelector = mkSelector "bookingTime"

-- | @Selector@ for @reservationStatus@
reservationStatusSelector :: Selector
reservationStatusSelector = mkSelector "reservationStatus"

-- | @Selector@ for @reservationHolderName@
reservationHolderNameSelector :: Selector
reservationHolderNameSelector = mkSelector "reservationHolderName"

-- | @Selector@ for @actions@
actionsSelector :: Selector
actionsSelector = mkSelector "actions"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"


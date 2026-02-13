{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSeat@.
module ObjC.Intents.INSeat
  ( INSeat
  , IsINSeat(..)
  , init_
  , initWithSeatSection_seatRow_seatNumber_seatingType
  , seatSection
  , seatRow
  , seatNumber
  , seatingType
  , initSelector
  , initWithSeatSection_seatRow_seatNumber_seatingTypeSelector
  , seatNumberSelector
  , seatRowSelector
  , seatSectionSelector
  , seatingTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSeat inSeat => inSeat -> IO (Id INSeat)
init_ inSeat =
  sendOwnedMessage inSeat initSelector

-- | @- initWithSeatSection:seatRow:seatNumber:seatingType:@
initWithSeatSection_seatRow_seatNumber_seatingType :: (IsINSeat inSeat, IsNSString seatSection, IsNSString seatRow, IsNSString seatNumber, IsNSString seatingType) => inSeat -> seatSection -> seatRow -> seatNumber -> seatingType -> IO (Id INSeat)
initWithSeatSection_seatRow_seatNumber_seatingType inSeat seatSection seatRow seatNumber seatingType =
  sendOwnedMessage inSeat initWithSeatSection_seatRow_seatNumber_seatingTypeSelector (toNSString seatSection) (toNSString seatRow) (toNSString seatNumber) (toNSString seatingType)

-- | @- seatSection@
seatSection :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatSection inSeat =
  sendMessage inSeat seatSectionSelector

-- | @- seatRow@
seatRow :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatRow inSeat =
  sendMessage inSeat seatRowSelector

-- | @- seatNumber@
seatNumber :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatNumber inSeat =
  sendMessage inSeat seatNumberSelector

-- | @- seatingType@
seatingType :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatingType inSeat =
  sendMessage inSeat seatingTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INSeat)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeatSection:seatRow:seatNumber:seatingType:@
initWithSeatSection_seatRow_seatNumber_seatingTypeSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString] (Id INSeat)
initWithSeatSection_seatRow_seatNumber_seatingTypeSelector = mkSelector "initWithSeatSection:seatRow:seatNumber:seatingType:"

-- | @Selector@ for @seatSection@
seatSectionSelector :: Selector '[] (Id NSString)
seatSectionSelector = mkSelector "seatSection"

-- | @Selector@ for @seatRow@
seatRowSelector :: Selector '[] (Id NSString)
seatRowSelector = mkSelector "seatRow"

-- | @Selector@ for @seatNumber@
seatNumberSelector :: Selector '[] (Id NSString)
seatNumberSelector = mkSelector "seatNumber"

-- | @Selector@ for @seatingType@
seatingTypeSelector :: Selector '[] (Id NSString)
seatingTypeSelector = mkSelector "seatingType"


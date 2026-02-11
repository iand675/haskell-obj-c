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
  , seatSectionSelector
  , seatRowSelector
  , seatNumberSelector
  , seatingTypeSelector


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

-- | @- init@
init_ :: IsINSeat inSeat => inSeat -> IO (Id INSeat)
init_ inSeat  =
  sendMsg inSeat (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSeatSection:seatRow:seatNumber:seatingType:@
initWithSeatSection_seatRow_seatNumber_seatingType :: (IsINSeat inSeat, IsNSString seatSection, IsNSString seatRow, IsNSString seatNumber, IsNSString seatingType) => inSeat -> seatSection -> seatRow -> seatNumber -> seatingType -> IO (Id INSeat)
initWithSeatSection_seatRow_seatNumber_seatingType inSeat  seatSection seatRow seatNumber seatingType =
withObjCPtr seatSection $ \raw_seatSection ->
  withObjCPtr seatRow $ \raw_seatRow ->
    withObjCPtr seatNumber $ \raw_seatNumber ->
      withObjCPtr seatingType $ \raw_seatingType ->
          sendMsg inSeat (mkSelector "initWithSeatSection:seatRow:seatNumber:seatingType:") (retPtr retVoid) [argPtr (castPtr raw_seatSection :: Ptr ()), argPtr (castPtr raw_seatRow :: Ptr ()), argPtr (castPtr raw_seatNumber :: Ptr ()), argPtr (castPtr raw_seatingType :: Ptr ())] >>= ownedObject . castPtr

-- | @- seatSection@
seatSection :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatSection inSeat  =
  sendMsg inSeat (mkSelector "seatSection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seatRow@
seatRow :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatRow inSeat  =
  sendMsg inSeat (mkSelector "seatRow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seatNumber@
seatNumber :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatNumber inSeat  =
  sendMsg inSeat (mkSelector "seatNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seatingType@
seatingType :: IsINSeat inSeat => inSeat -> IO (Id NSString)
seatingType inSeat  =
  sendMsg inSeat (mkSelector "seatingType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSeatSection:seatRow:seatNumber:seatingType:@
initWithSeatSection_seatRow_seatNumber_seatingTypeSelector :: Selector
initWithSeatSection_seatRow_seatNumber_seatingTypeSelector = mkSelector "initWithSeatSection:seatRow:seatNumber:seatingType:"

-- | @Selector@ for @seatSection@
seatSectionSelector :: Selector
seatSectionSelector = mkSelector "seatSection"

-- | @Selector@ for @seatRow@
seatRowSelector :: Selector
seatRowSelector = mkSelector "seatRow"

-- | @Selector@ for @seatNumber@
seatNumberSelector :: Selector
seatNumberSelector = mkSelector "seatNumber"

-- | @Selector@ for @seatingType@
seatingTypeSelector :: Selector
seatingTypeSelector = mkSelector "seatingType"


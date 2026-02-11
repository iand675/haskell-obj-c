{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents single slot which can contain SmartCard.
--
-- Generated bindings for @TKSmartCardSlot@.
module ObjC.CryptoTokenKit.TKSmartCardSlot
  ( TKSmartCardSlot
  , IsTKSmartCardSlot(..)
  , makeSmartCard
  , state
  , atr
  , name
  , maxInputLength
  , maxOutputLength
  , makeSmartCardSelector
  , stateSelector
  , atrSelector
  , nameSelector
  , maxInputLengthSelector
  , maxOutputLengthSelector

  -- * Enum types
  , TKSmartCardSlotState(TKSmartCardSlotState)
  , pattern TKSmartCardSlotStateMissing
  , pattern TKSmartCardSlotStateEmpty
  , pattern TKSmartCardSlotStateProbing
  , pattern TKSmartCardSlotStateMuteCard
  , pattern TKSmartCardSlotStateValidCard

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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates new object representing currently inserted and valid card.
--
-- It is possible to instantiate multiple objects for single card, exclusivity is handled by sessions on the level of created SmartCard objects.
--
-- Returns: Newly created SmartCard object, or nil if slot does not contain valid card.
--
-- ObjC selector: @- makeSmartCard@
makeSmartCard :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO (Id TKSmartCard)
makeSmartCard tkSmartCardSlot  =
  sendMsg tkSmartCardSlot (mkSelector "makeSmartCard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Current state of the slot.  Use KVO to be notified about state changes.
--
-- ObjC selector: @- state@
state :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO TKSmartCardSlotState
state tkSmartCardSlot  =
  fmap (coerce :: CLong -> TKSmartCardSlotState) $ sendMsg tkSmartCardSlot (mkSelector "state") retCLong []

-- | ATR of the inserted SmartCard, or nil if no or mute SmartCard is inserted.
--
-- ObjC selector: @- ATR@
atr :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO (Id TKSmartCardATR)
atr tkSmartCardSlot  =
  sendMsg tkSmartCardSlot (mkSelector "ATR") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of the SmartCard reader slot.
--
-- ObjC selector: @- name@
name :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO (Id NSString)
name tkSmartCardSlot  =
  sendMsg tkSmartCardSlot (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Maximal length of input APDU that the slot is able to transfer to the card.
--
-- ObjC selector: @- maxInputLength@
maxInputLength :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO CLong
maxInputLength tkSmartCardSlot  =
  sendMsg tkSmartCardSlot (mkSelector "maxInputLength") retCLong []

-- | Maximal length of output APDU that the slot is able to transfer from the card.
--
-- ObjC selector: @- maxOutputLength@
maxOutputLength :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO CLong
maxOutputLength tkSmartCardSlot  =
  sendMsg tkSmartCardSlot (mkSelector "maxOutputLength") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeSmartCard@
makeSmartCardSelector :: Selector
makeSmartCardSelector = mkSelector "makeSmartCard"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @ATR@
atrSelector :: Selector
atrSelector = mkSelector "ATR"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @maxInputLength@
maxInputLengthSelector :: Selector
maxInputLengthSelector = mkSelector "maxInputLength"

-- | @Selector@ for @maxOutputLength@
maxOutputLengthSelector :: Selector
maxOutputLengthSelector = mkSelector "maxOutputLength"


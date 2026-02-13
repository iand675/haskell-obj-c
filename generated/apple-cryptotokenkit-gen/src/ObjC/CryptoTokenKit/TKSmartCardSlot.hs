{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , atrSelector
  , makeSmartCardSelector
  , maxInputLengthSelector
  , maxOutputLengthSelector
  , nameSelector
  , stateSelector

  -- * Enum types
  , TKSmartCardSlotState(TKSmartCardSlotState)
  , pattern TKSmartCardSlotStateMissing
  , pattern TKSmartCardSlotStateEmpty
  , pattern TKSmartCardSlotStateProbing
  , pattern TKSmartCardSlotStateMuteCard
  , pattern TKSmartCardSlotStateValidCard

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
makeSmartCard tkSmartCardSlot =
  sendMessage tkSmartCardSlot makeSmartCardSelector

-- | Current state of the slot.  Use KVO to be notified about state changes.
--
-- ObjC selector: @- state@
state :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO TKSmartCardSlotState
state tkSmartCardSlot =
  sendMessage tkSmartCardSlot stateSelector

-- | ATR of the inserted SmartCard, or nil if no or mute SmartCard is inserted.
--
-- ObjC selector: @- ATR@
atr :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO (Id TKSmartCardATR)
atr tkSmartCardSlot =
  sendMessage tkSmartCardSlot atrSelector

-- | Name of the SmartCard reader slot.
--
-- ObjC selector: @- name@
name :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO (Id NSString)
name tkSmartCardSlot =
  sendMessage tkSmartCardSlot nameSelector

-- | Maximal length of input APDU that the slot is able to transfer to the card.
--
-- ObjC selector: @- maxInputLength@
maxInputLength :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO CLong
maxInputLength tkSmartCardSlot =
  sendMessage tkSmartCardSlot maxInputLengthSelector

-- | Maximal length of output APDU that the slot is able to transfer from the card.
--
-- ObjC selector: @- maxOutputLength@
maxOutputLength :: IsTKSmartCardSlot tkSmartCardSlot => tkSmartCardSlot -> IO CLong
maxOutputLength tkSmartCardSlot =
  sendMessage tkSmartCardSlot maxOutputLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeSmartCard@
makeSmartCardSelector :: Selector '[] (Id TKSmartCard)
makeSmartCardSelector = mkSelector "makeSmartCard"

-- | @Selector@ for @state@
stateSelector :: Selector '[] TKSmartCardSlotState
stateSelector = mkSelector "state"

-- | @Selector@ for @ATR@
atrSelector :: Selector '[] (Id TKSmartCardATR)
atrSelector = mkSelector "ATR"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @maxInputLength@
maxInputLengthSelector :: Selector '[] CLong
maxInputLengthSelector = mkSelector "maxInputLength"

-- | @Selector@ for @maxOutputLength@
maxOutputLengthSelector :: Selector '[] CLong
maxOutputLengthSelector = mkSelector "maxOutputLength"


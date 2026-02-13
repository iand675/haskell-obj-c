{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCRacingWheelInputState@.
module ObjC.GameController.GCRacingWheelInputState
  ( GCRacingWheelInputState
  , IsGCRacingWheelInputState(..)
  , wheel
  , acceleratorPedal
  , brakePedal
  , clutchPedal
  , shifter
  , acceleratorPedalSelector
  , brakePedalSelector
  , clutchPedalSelector
  , shifterSelector
  , wheelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The steering wheel element.
--
-- ObjC selector: @- wheel@
wheel :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO (Id GCSteeringWheelElement)
wheel gcRacingWheelInputState =
  sendMessage gcRacingWheelInputState wheelSelector

-- | @- acceleratorPedal@
acceleratorPedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
acceleratorPedal gcRacingWheelInputState =
  sendMessage gcRacingWheelInputState acceleratorPedalSelector

-- | @- brakePedal@
brakePedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
brakePedal gcRacingWheelInputState =
  sendMessage gcRacingWheelInputState brakePedalSelector

-- | @- clutchPedal@
clutchPedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
clutchPedal gcRacingWheelInputState =
  sendMessage gcRacingWheelInputState clutchPedalSelector

-- | The element representing an attached gear shifter accessory.
--
-- Note that this element only represents an external gear shifter accessory. Many racing wheels have a pair of built in paddle buttons that can be used for sequential gear shifting.  Those buttons are can be looked up with the @GCInputLeftPaddle@ and @GCInputRightPaddle@ input names.
--
-- ObjC selector: @- shifter@
shifter :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO (Id GCGearShifterElement)
shifter gcRacingWheelInputState =
  sendMessage gcRacingWheelInputState shifterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheel@
wheelSelector :: Selector '[] (Id GCSteeringWheelElement)
wheelSelector = mkSelector "wheel"

-- | @Selector@ for @acceleratorPedal@
acceleratorPedalSelector :: Selector '[] RawId
acceleratorPedalSelector = mkSelector "acceleratorPedal"

-- | @Selector@ for @brakePedal@
brakePedalSelector :: Selector '[] RawId
brakePedalSelector = mkSelector "brakePedal"

-- | @Selector@ for @clutchPedal@
clutchPedalSelector :: Selector '[] RawId
clutchPedalSelector = mkSelector "clutchPedal"

-- | @Selector@ for @shifter@
shifterSelector :: Selector '[] (Id GCGearShifterElement)
shifterSelector = mkSelector "shifter"


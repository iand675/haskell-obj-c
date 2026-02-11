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
  , wheelSelector
  , acceleratorPedalSelector
  , brakePedalSelector
  , clutchPedalSelector
  , shifterSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The steering wheel element.
--
-- ObjC selector: @- wheel@
wheel :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO (Id GCSteeringWheelElement)
wheel gcRacingWheelInputState  =
    sendMsg gcRacingWheelInputState (mkSelector "wheel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- acceleratorPedal@
acceleratorPedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
acceleratorPedal gcRacingWheelInputState  =
    fmap (RawId . castPtr) $ sendMsg gcRacingWheelInputState (mkSelector "acceleratorPedal") (retPtr retVoid) []

-- | @- brakePedal@
brakePedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
brakePedal gcRacingWheelInputState  =
    fmap (RawId . castPtr) $ sendMsg gcRacingWheelInputState (mkSelector "brakePedal") (retPtr retVoid) []

-- | @- clutchPedal@
clutchPedal :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO RawId
clutchPedal gcRacingWheelInputState  =
    fmap (RawId . castPtr) $ sendMsg gcRacingWheelInputState (mkSelector "clutchPedal") (retPtr retVoid) []

-- | The element representing an attached gear shifter accessory.
--
-- Note that this element only represents an external gear shifter accessory. Many racing wheels have a pair of built in paddle buttons that can be used for sequential gear shifting.  Those buttons are can be looked up with the @GCInputLeftPaddle@ and @GCInputRightPaddle@ input names.
--
-- ObjC selector: @- shifter@
shifter :: IsGCRacingWheelInputState gcRacingWheelInputState => gcRacingWheelInputState -> IO (Id GCGearShifterElement)
shifter gcRacingWheelInputState  =
    sendMsg gcRacingWheelInputState (mkSelector "shifter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheel@
wheelSelector :: Selector
wheelSelector = mkSelector "wheel"

-- | @Selector@ for @acceleratorPedal@
acceleratorPedalSelector :: Selector
acceleratorPedalSelector = mkSelector "acceleratorPedal"

-- | @Selector@ for @brakePedal@
brakePedalSelector :: Selector
brakePedalSelector = mkSelector "brakePedal"

-- | @Selector@ for @clutchPedal@
clutchPedalSelector :: Selector
clutchPedalSelector = mkSelector "clutchPedal"

-- | @Selector@ for @shifter@
shifterSelector :: Selector
shifterSelector = mkSelector "shifter"


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSystemPressureState
--
-- A model object describing a system pressure level and contributing factors to the pressured state.
--
-- Beginning in iOS 11.1, AVCaptureDevice can report its current system pressure state. System pressure refers to a state in which capture quality is degraded or capture hardware availability is limited due to factors such as overall system temperature, insufficient battery charge for current peak power requirements, or camera module temperature.
--
-- Generated bindings for @AVCaptureSystemPressureState@.
module ObjC.AVFoundation.AVCaptureSystemPressureState
  ( AVCaptureSystemPressureState
  , IsAVCaptureSystemPressureState(..)
  , init_
  , new
  , level
  , factors
  , initSelector
  , newSelector
  , levelSelector
  , factorsSelector

  -- * Enum types
  , AVCaptureSystemPressureFactors(AVCaptureSystemPressureFactors)
  , pattern AVCaptureSystemPressureFactorNone
  , pattern AVCaptureSystemPressureFactorSystemTemperature
  , pattern AVCaptureSystemPressureFactorPeakPower
  , pattern AVCaptureSystemPressureFactorDepthModuleTemperature
  , pattern AVCaptureSystemPressureFactorCameraTemperature

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO (Id AVCaptureSystemPressureState)
init_ avCaptureSystemPressureState  =
  sendMsg avCaptureSystemPressureState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureSystemPressureState)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSystemPressureState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | level
--
-- An enumerated string value characterizing the pressure level to which the system is currently elevated.
--
-- ObjC selector: @- level@
level :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO (Id NSString)
level avCaptureSystemPressureState  =
  sendMsg avCaptureSystemPressureState (mkSelector "level") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | factors
--
-- A bitmask of values indicating the factors contributing to the current system pressure level.
--
-- ObjC selector: @- factors@
factors :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO AVCaptureSystemPressureFactors
factors avCaptureSystemPressureState  =
  fmap (coerce :: CULong -> AVCaptureSystemPressureFactors) $ sendMsg avCaptureSystemPressureState (mkSelector "factors") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @factors@
factorsSelector :: Selector
factorsSelector = mkSelector "factors"


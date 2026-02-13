{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , factorsSelector
  , initSelector
  , levelSelector
  , newSelector

  -- * Enum types
  , AVCaptureSystemPressureFactors(AVCaptureSystemPressureFactors)
  , pattern AVCaptureSystemPressureFactorNone
  , pattern AVCaptureSystemPressureFactorSystemTemperature
  , pattern AVCaptureSystemPressureFactorPeakPower
  , pattern AVCaptureSystemPressureFactorDepthModuleTemperature
  , pattern AVCaptureSystemPressureFactorCameraTemperature

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO (Id AVCaptureSystemPressureState)
init_ avCaptureSystemPressureState =
  sendOwnedMessage avCaptureSystemPressureState initSelector

-- | @+ new@
new :: IO (Id AVCaptureSystemPressureState)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSystemPressureState"
    sendOwnedClassMessage cls' newSelector

-- | level
--
-- An enumerated string value characterizing the pressure level to which the system is currently elevated.
--
-- ObjC selector: @- level@
level :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO (Id NSString)
level avCaptureSystemPressureState =
  sendMessage avCaptureSystemPressureState levelSelector

-- | factors
--
-- A bitmask of values indicating the factors contributing to the current system pressure level.
--
-- ObjC selector: @- factors@
factors :: IsAVCaptureSystemPressureState avCaptureSystemPressureState => avCaptureSystemPressureState -> IO AVCaptureSystemPressureFactors
factors avCaptureSystemPressureState =
  sendMessage avCaptureSystemPressureState factorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureSystemPressureState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureSystemPressureState)
newSelector = mkSelector "new"

-- | @Selector@ for @level@
levelSelector :: Selector '[] (Id NSString)
levelSelector = mkSelector "level"

-- | @Selector@ for @factors@
factorsSelector :: Selector '[] AVCaptureSystemPressureFactors
factorsSelector = mkSelector "factors"


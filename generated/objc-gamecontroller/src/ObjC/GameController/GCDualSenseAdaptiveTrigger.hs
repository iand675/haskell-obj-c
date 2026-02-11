{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DualSense triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- GCDualSenseAdaptiveTrigger represents an adaptive trigger on the Sony DualSense controller, allowing you to specify a dynamic resistance force that is applied when pulling the trigger. This can, for example, be used to emulate the feeling of pulling back a bow string, firing a weapon, or pulling a lever.
--
-- See: GCDualSenseGamepad
--
-- Generated bindings for @GCDualSenseAdaptiveTrigger@.
module ObjC.GameController.GCDualSenseAdaptiveTrigger
  ( GCDualSenseAdaptiveTrigger
  , IsGCDualSenseAdaptiveTrigger(..)
  , setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrength
  , setModeFeedbackWithStartPosition_resistiveStrength
  , setModeWeaponWithStartPosition_endPosition_resistiveStrength
  , setModeVibrationWithStartPosition_amplitude_frequency
  , setModeOff
  , mode
  , status
  , armPosition
  , setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrengthSelector
  , setModeFeedbackWithStartPosition_resistiveStrengthSelector
  , setModeWeaponWithStartPosition_endPosition_resistiveStrengthSelector
  , setModeVibrationWithStartPosition_amplitude_frequencySelector
  , setModeOffSelector
  , modeSelector
  , statusSelector
  , armPositionSelector

  -- * Enum types
  , GCDualSenseAdaptiveTriggerMode(GCDualSenseAdaptiveTriggerMode)
  , pattern GCDualSenseAdaptiveTriggerModeOff
  , pattern GCDualSenseAdaptiveTriggerModeFeedback
  , pattern GCDualSenseAdaptiveTriggerModeWeapon
  , pattern GCDualSenseAdaptiveTriggerModeVibration
  , pattern GCDualSenseAdaptiveTriggerModeSlopeFeedback
  , GCDualSenseAdaptiveTriggerStatus(GCDualSenseAdaptiveTriggerStatus)
  , pattern GCDualSenseAdaptiveTriggerStatusUnknown
  , pattern GCDualSenseAdaptiveTriggerStatusFeedbackNoLoad
  , pattern GCDualSenseAdaptiveTriggerStatusFeedbackLoadApplied
  , pattern GCDualSenseAdaptiveTriggerStatusWeaponReady
  , pattern GCDualSenseAdaptiveTriggerStatusWeaponFiring
  , pattern GCDualSenseAdaptiveTriggerStatusWeaponFired
  , pattern GCDualSenseAdaptiveTriggerStatusVibrationNotVibrating
  , pattern GCDualSenseAdaptiveTriggerStatusVibrationIsVibrating
  , pattern GCDualSenseAdaptiveTriggerStatusSlopeFeedbackReady
  , pattern GCDualSenseAdaptiveTriggerStatusSlopeFeedbackApplyingLoad
  , pattern GCDualSenseAdaptiveTriggerStatusSlopeFeedbackFinished

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
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Sets the adaptive trigger to slope feedback mode. The start position, end position, start strength, and end strength of the effect can be set arbitrarily; however the end position must be larger than the start position. The trigger arm will provide a linearly interpolated degree of feedback whenever it is depressed between the start and end positions based on the starting and ending strengths.
--
-- @startPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible trigger depression and 1 representing the maximum trigger depression. The effect will begin once the trigger is depressed beyond this point.
--
-- @endPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible depression and 1 representing the maximum trigger depression. Must be greater than startPosition. The effect will end once the trigger is depressed beyond this point.
--
-- @startStrength@ — - A normalized float from [0-1], with 0 representing the minimum effect strength (off entirely) and 1 representing the maximum effect strength. The effect will begin at startStrength once the trigger is depressed beyond startPosition.
--
-- @endStrength@ — - A normalized float from [0-1], with 0 representing the minimum effect strength (off entirely) and 1 representing the maximum effect strength. The effect will end at endStrength once the trigger is depressed to endPosition.
--
-- ObjC selector: @- setModeSlopeFeedbackWithStartPosition:endPosition:startStrength:endStrength:@
setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrength :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrength gcDualSenseAdaptiveTrigger  startPosition endPosition startStrength endStrength =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "setModeSlopeFeedbackWithStartPosition:endPosition:startStrength:endStrength:") retVoid [argCFloat (fromIntegral startPosition), argCFloat (fromIntegral endPosition), argCFloat (fromIntegral startStrength), argCFloat (fromIntegral endStrength)]

-- | Sets the adaptive trigger to feedback mode. The start position and strength of the effect can be set arbitrarily. The trigger arm will continue to provide a constant degree of feedback whenever it is depressed further than the start position.
--
-- @startPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible trigger depression and 1 representing the maximum trigger depression.
--
-- @resistiveStrength@ — - A normalized float from [0-1], with 0 representing the minimum effect strength (off entirely) and 1 representing the maximum effect strength.
--
-- ObjC selector: @- setModeFeedbackWithStartPosition:resistiveStrength:@
setModeFeedbackWithStartPosition_resistiveStrength :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> CFloat -> CFloat -> IO ()
setModeFeedbackWithStartPosition_resistiveStrength gcDualSenseAdaptiveTrigger  startPosition resistiveStrength =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "setModeFeedbackWithStartPosition:resistiveStrength:") retVoid [argCFloat (fromIntegral startPosition), argCFloat (fromIntegral resistiveStrength)]

-- | Sets the adaptive trigger to weapon mode. The start position, end position, and strength of the effect can be set arbitrarily; however the end position must be larger than the start position. The trigger arm will continue to provide a constant degree of feedback whenever it is depressed further than the start position. Once the trigger arm has been depressed past the end position, the strength of the effect will immediately fall to zero, providing a "sense of release" similar to that provided by pulling the trigger of a weapon.
--
-- @startPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible depression and 1 representing the maximum trigger depression. The effect will begin once the trigger is depressed beyond this point.
--
-- @endPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible depression and 1 representing the maximum trigger depression. Must be greater than startPosition. The effect will end once the trigger is depressed beyond this point.
--
-- @resistiveStrength@ — - A normalized float from [0-1], with 0 representing the minimum effect strength (off entirely) and 1 representing the maximum effect strength.
--
-- ObjC selector: @- setModeWeaponWithStartPosition:endPosition:resistiveStrength:@
setModeWeaponWithStartPosition_endPosition_resistiveStrength :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> CFloat -> CFloat -> CFloat -> IO ()
setModeWeaponWithStartPosition_endPosition_resistiveStrength gcDualSenseAdaptiveTrigger  startPosition endPosition resistiveStrength =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "setModeWeaponWithStartPosition:endPosition:resistiveStrength:") retVoid [argCFloat (fromIntegral startPosition), argCFloat (fromIntegral endPosition), argCFloat (fromIntegral resistiveStrength)]

-- | Sets the adaptive trigger to vibration mode. The start position, amplitude, and frequency of the effect can be set arbitrarily. The trigger arm will continue to strike against the trigger whenever it is depressed further than the start position, providing a "sense of vibration".
--
-- @startPosition@ — - A normalized float from [0-1], with 0 representing the smallest possible depression and 1 representing the maximum trigger depression. The effect will begin once the trigger is depressed beyond this point.
--
-- @amplitude@ — - A normalized float from [0-1], with 0 representing the minimum effect strength (off entirely) and 1 representing the maximum effect strength.
--
-- @frequency@ — - A normalized float from [0-1], with 0 representing the minimum frequency and 1 representing the maximum frequency of the vibration effect.
--
-- ObjC selector: @- setModeVibrationWithStartPosition:amplitude:frequency:@
setModeVibrationWithStartPosition_amplitude_frequency :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> CFloat -> CFloat -> CFloat -> IO ()
setModeVibrationWithStartPosition_amplitude_frequency gcDualSenseAdaptiveTrigger  startPosition amplitude frequency =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "setModeVibrationWithStartPosition:amplitude:frequency:") retVoid [argCFloat (fromIntegral startPosition), argCFloat (fromIntegral amplitude), argCFloat (fromIntegral frequency)]

-- | Sets the adaptive trigger to off mode. This turns off the adaptive trigger effect.
--
-- ObjC selector: @- setModeOff@
setModeOff :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> IO ()
setModeOff gcDualSenseAdaptiveTrigger  =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "setModeOff") retVoid []

-- | The mode that the adaptive trigger is currently in. This property reflects the physical state of the triggers - and requires a response from the controller. It does not update immediately after calling -[GCDualSenseAdaptiveTrigger setMode...].
--
-- See: GCDualSenseAdaptiveTriggerMode
--
-- ObjC selector: @- mode@
mode :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> IO GCDualSenseAdaptiveTriggerMode
mode gcDualSenseAdaptiveTrigger  =
  fmap (coerce :: CLong -> GCDualSenseAdaptiveTriggerMode) $ sendMsg gcDualSenseAdaptiveTrigger (mkSelector "mode") retCLong []

-- | The current status of the adaptive trigger - whether it is ready to apply a load, is currently applying a load, or has finished applying a load.
--
-- See: GCDualSenseAdaptiveTriggerStatus
--
-- ObjC selector: @- status@
status :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> IO GCDualSenseAdaptiveTriggerStatus
status gcDualSenseAdaptiveTrigger  =
  fmap (coerce :: CLong -> GCDualSenseAdaptiveTriggerStatus) $ sendMsg gcDualSenseAdaptiveTrigger (mkSelector "status") retCLong []

-- | A normalized float from [0-1], with 0 representing the lowest possible trigger arm position and 1 representing the maximum trigger arm position.
--
-- See: GCDualSenseAdaptiveTriggerStatus
--
-- ObjC selector: @- armPosition@
armPosition :: IsGCDualSenseAdaptiveTrigger gcDualSenseAdaptiveTrigger => gcDualSenseAdaptiveTrigger -> IO CFloat
armPosition gcDualSenseAdaptiveTrigger  =
  sendMsg gcDualSenseAdaptiveTrigger (mkSelector "armPosition") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setModeSlopeFeedbackWithStartPosition:endPosition:startStrength:endStrength:@
setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrengthSelector :: Selector
setModeSlopeFeedbackWithStartPosition_endPosition_startStrength_endStrengthSelector = mkSelector "setModeSlopeFeedbackWithStartPosition:endPosition:startStrength:endStrength:"

-- | @Selector@ for @setModeFeedbackWithStartPosition:resistiveStrength:@
setModeFeedbackWithStartPosition_resistiveStrengthSelector :: Selector
setModeFeedbackWithStartPosition_resistiveStrengthSelector = mkSelector "setModeFeedbackWithStartPosition:resistiveStrength:"

-- | @Selector@ for @setModeWeaponWithStartPosition:endPosition:resistiveStrength:@
setModeWeaponWithStartPosition_endPosition_resistiveStrengthSelector :: Selector
setModeWeaponWithStartPosition_endPosition_resistiveStrengthSelector = mkSelector "setModeWeaponWithStartPosition:endPosition:resistiveStrength:"

-- | @Selector@ for @setModeVibrationWithStartPosition:amplitude:frequency:@
setModeVibrationWithStartPosition_amplitude_frequencySelector :: Selector
setModeVibrationWithStartPosition_amplitude_frequencySelector = mkSelector "setModeVibrationWithStartPosition:amplitude:frequency:"

-- | @Selector@ for @setModeOff@
setModeOffSelector :: Selector
setModeOffSelector = mkSelector "setModeOff"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @armPosition@
armPositionSelector :: Selector
armPositionSelector = mkSelector "armPosition"


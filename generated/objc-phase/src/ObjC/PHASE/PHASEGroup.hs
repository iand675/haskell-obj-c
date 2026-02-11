{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGroup
--
-- A PHASEGroup allows clients to group generator nodes for shared processing.        Clients can set the gain and playback rate, as well as mute and solo the generator nodes in a group.
--
-- Generated bindings for @PHASEGroup@.
module ObjC.PHASE.PHASEGroup
  ( PHASEGroup
  , IsPHASEGroup(..)
  , init_
  , new
  , initWithIdentifier
  , registerWithEngine
  , unregisterFromEngine
  , fadeGain_duration_curveType
  , fadeRate_duration_curveType
  , mute
  , unmute
  , solo
  , unsolo
  , identifier
  , gain
  , setGain
  , rate
  , setRate
  , muted
  , soloed
  , initSelector
  , newSelector
  , initWithIdentifierSelector
  , registerWithEngineSelector
  , unregisterFromEngineSelector
  , fadeGain_duration_curveTypeSelector
  , fadeRate_duration_curveTypeSelector
  , muteSelector
  , unmuteSelector
  , soloSelector
  , unsoloSelector
  , identifierSelector
  , gainSelector
  , setGainSelector
  , rateSelector
  , setRateSelector
  , mutedSelector
  , soloedSelector

  -- * Enum types
  , PHASECurveType(PHASECurveType)
  , pattern PHASECurveTypeLinear
  , pattern PHASECurveTypeSquared
  , pattern PHASECurveTypeInverseSquared
  , pattern PHASECurveTypeCubed
  , pattern PHASECurveTypeInverseCubed
  , pattern PHASECurveTypeSine
  , pattern PHASECurveTypeInverseSine
  , pattern PHASECurveTypeSigmoid
  , pattern PHASECurveTypeInverseSigmoid
  , pattern PHASECurveTypeHoldStartValue
  , pattern PHASECurveTypeJumpToEndValue

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

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGroup phaseGroup => phaseGroup -> IO (Id PHASEGroup)
init_ phaseGroup  =
  sendMsg phaseGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEGroup)
new  =
  do
    cls' <- getRequiredClass "PHASEGroup"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithIdentifier:
--
-- Create a new group.
--
-- @identifier@ — The identifier that uniquely represents this group.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsPHASEGroup phaseGroup, IsNSString identifier) => phaseGroup -> identifier -> IO (Id PHASEGroup)
initWithIdentifier phaseGroup  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg phaseGroup (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | registerWithEngine
--
-- Registers a group with a particular engine so that referenced assets can find it.
--
-- Note: An exception will be thrown if the engine is invalid or the group already exists.
--
-- @engine@ — An engine object to associate this group with.
--
-- ObjC selector: @- registerWithEngine:@
registerWithEngine :: (IsPHASEGroup phaseGroup, IsPHASEEngine engine) => phaseGroup -> engine -> IO ()
registerWithEngine phaseGroup  engine =
withObjCPtr engine $ \raw_engine ->
    sendMsg phaseGroup (mkSelector "registerWithEngine:") retVoid [argPtr (castPtr raw_engine :: Ptr ())]

-- | unregisterFromEngine
--
-- Unregister the group from a particular engine.
--
-- ObjC selector: @- unregisterFromEngine@
unregisterFromEngine :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unregisterFromEngine phaseGroup  =
  sendMsg phaseGroup (mkSelector "unregisterFromEngine") retVoid []

-- | fadeGain:duration:curveType:
--
-- Fade the gain of this group over a specified duration and curve.
--
-- Note: The fade gain is applied on top of the base gain of the group.
--
-- @gain@ — A target linear gain scalar. Values are clamped to the range [0, 1].
--
-- @duration@ — The duration over which to ramp to the target linear gain scalar. Values must be >= 0.        The duration is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- @curveType@ — The type of curve function that is applied during the fade.
--
-- ObjC selector: @- fadeGain:duration:curveType:@
fadeGain_duration_curveType :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> CDouble -> PHASECurveType -> IO ()
fadeGain_duration_curveType phaseGroup  gain duration curveType =
  sendMsg phaseGroup (mkSelector "fadeGain:duration:curveType:") retVoid [argCDouble (fromIntegral gain), argCDouble (fromIntegral duration), argCLong (coerce curveType)]

-- | fadeRate:duration:curveType:
--
-- Fade the playback rate of this group over a specified duration and curve.
--
-- Note: The fade gain is applied on top of the base gain of the group.
--
-- @rate@ — A target linear rate scalar. Values are clamped to the range [0.25, 4.0].
--
-- @duration@ — The duration over which to ramp to the target linear rate scalar. Values must be >= 0.        The duration is scaled by unitsPerSecond internally, so can be provided at the client's native time scale.
--
-- @curveType@ — The type of curve function that is applied during the fade.
--
-- ObjC selector: @- fadeRate:duration:curveType:@
fadeRate_duration_curveType :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> CDouble -> PHASECurveType -> IO ()
fadeRate_duration_curveType phaseGroup  rate duration curveType =
  sendMsg phaseGroup (mkSelector "fadeRate:duration:curveType:") retVoid [argCDouble (fromIntegral rate), argCDouble (fromIntegral duration), argCLong (coerce curveType)]

-- | mute
--
-- Mute the group.
--
-- ObjC selector: @- mute@
mute :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
mute phaseGroup  =
  sendMsg phaseGroup (mkSelector "mute") retVoid []

-- | unmute
--
-- Unmute the group.
--
-- ObjC selector: @- unmute@
unmute :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unmute phaseGroup  =
  sendMsg phaseGroup (mkSelector "unmute") retVoid []

-- | solo
--
-- Solo the group.
--
-- ObjC selector: @- solo@
solo :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
solo phaseGroup  =
  sendMsg phaseGroup (mkSelector "solo") retVoid []

-- | unsolo
--
-- Unsolo the group.
--
-- ObjC selector: @- unsolo@
unsolo :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unsolo phaseGroup  =
  sendMsg phaseGroup (mkSelector "unsolo") retVoid []

-- | identifier
--
-- The identifier that uniquely represents this group.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEGroup phaseGroup => phaseGroup -> IO (Id NSString)
identifier phaseGroup  =
  sendMsg phaseGroup (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEGroup phaseGroup => phaseGroup -> IO CDouble
gain phaseGroup  =
  sendMsg phaseGroup (mkSelector "gain") retCDouble []

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> IO ()
setGain phaseGroup  value =
  sendMsg phaseGroup (mkSelector "setGain:") retVoid [argCDouble (fromIntegral value)]

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGroup phaseGroup => phaseGroup -> IO CDouble
rate phaseGroup  =
  sendMsg phaseGroup (mkSelector "rate") retCDouble []

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- setRate:@
setRate :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> IO ()
setRate phaseGroup  value =
  sendMsg phaseGroup (mkSelector "setRate:") retVoid [argCDouble (fromIntegral value)]

-- | muted
--
-- Whether or not this group is muted.
--
-- ObjC selector: @- muted@
muted :: IsPHASEGroup phaseGroup => phaseGroup -> IO Bool
muted phaseGroup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseGroup (mkSelector "muted") retCULong []

-- | soloed
--
-- Whether or not this group is soloed.
--
-- ObjC selector: @- soloed@
soloed :: IsPHASEGroup phaseGroup => phaseGroup -> IO Bool
soloed phaseGroup  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phaseGroup (mkSelector "soloed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @registerWithEngine:@
registerWithEngineSelector :: Selector
registerWithEngineSelector = mkSelector "registerWithEngine:"

-- | @Selector@ for @unregisterFromEngine@
unregisterFromEngineSelector :: Selector
unregisterFromEngineSelector = mkSelector "unregisterFromEngine"

-- | @Selector@ for @fadeGain:duration:curveType:@
fadeGain_duration_curveTypeSelector :: Selector
fadeGain_duration_curveTypeSelector = mkSelector "fadeGain:duration:curveType:"

-- | @Selector@ for @fadeRate:duration:curveType:@
fadeRate_duration_curveTypeSelector :: Selector
fadeRate_duration_curveTypeSelector = mkSelector "fadeRate:duration:curveType:"

-- | @Selector@ for @mute@
muteSelector :: Selector
muteSelector = mkSelector "mute"

-- | @Selector@ for @unmute@
unmuteSelector :: Selector
unmuteSelector = mkSelector "unmute"

-- | @Selector@ for @solo@
soloSelector :: Selector
soloSelector = mkSelector "solo"

-- | @Selector@ for @unsolo@
unsoloSelector :: Selector
unsoloSelector = mkSelector "unsolo"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @muted@
mutedSelector :: Selector
mutedSelector = mkSelector "muted"

-- | @Selector@ for @soloed@
soloedSelector :: Selector
soloedSelector = mkSelector "soloed"


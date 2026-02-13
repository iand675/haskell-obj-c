{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , fadeGain_duration_curveTypeSelector
  , fadeRate_duration_curveTypeSelector
  , gainSelector
  , identifierSelector
  , initSelector
  , initWithIdentifierSelector
  , muteSelector
  , mutedSelector
  , newSelector
  , rateSelector
  , registerWithEngineSelector
  , setGainSelector
  , setRateSelector
  , soloSelector
  , soloedSelector
  , unmuteSelector
  , unregisterFromEngineSelector
  , unsoloSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.PHASE.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGroup phaseGroup => phaseGroup -> IO (Id PHASEGroup)
init_ phaseGroup =
  sendOwnedMessage phaseGroup initSelector

-- | @+ new@
new :: IO (Id PHASEGroup)
new  =
  do
    cls' <- getRequiredClass "PHASEGroup"
    sendOwnedClassMessage cls' newSelector

-- | initWithIdentifier:
--
-- Create a new group.
--
-- @identifier@ — The identifier that uniquely represents this group.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsPHASEGroup phaseGroup, IsNSString identifier) => phaseGroup -> identifier -> IO (Id PHASEGroup)
initWithIdentifier phaseGroup identifier =
  sendOwnedMessage phaseGroup initWithIdentifierSelector (toNSString identifier)

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
registerWithEngine phaseGroup engine =
  sendMessage phaseGroup registerWithEngineSelector (toPHASEEngine engine)

-- | unregisterFromEngine
--
-- Unregister the group from a particular engine.
--
-- ObjC selector: @- unregisterFromEngine@
unregisterFromEngine :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unregisterFromEngine phaseGroup =
  sendMessage phaseGroup unregisterFromEngineSelector

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
fadeGain_duration_curveType phaseGroup gain duration curveType =
  sendMessage phaseGroup fadeGain_duration_curveTypeSelector gain duration curveType

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
fadeRate_duration_curveType phaseGroup rate duration curveType =
  sendMessage phaseGroup fadeRate_duration_curveTypeSelector rate duration curveType

-- | mute
--
-- Mute the group.
--
-- ObjC selector: @- mute@
mute :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
mute phaseGroup =
  sendMessage phaseGroup muteSelector

-- | unmute
--
-- Unmute the group.
--
-- ObjC selector: @- unmute@
unmute :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unmute phaseGroup =
  sendMessage phaseGroup unmuteSelector

-- | solo
--
-- Solo the group.
--
-- ObjC selector: @- solo@
solo :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
solo phaseGroup =
  sendMessage phaseGroup soloSelector

-- | unsolo
--
-- Unsolo the group.
--
-- ObjC selector: @- unsolo@
unsolo :: IsPHASEGroup phaseGroup => phaseGroup -> IO ()
unsolo phaseGroup =
  sendMessage phaseGroup unsoloSelector

-- | identifier
--
-- The identifier that uniquely represents this group.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEGroup phaseGroup => phaseGroup -> IO (Id NSString)
identifier phaseGroup =
  sendMessage phaseGroup identifierSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- gain@
gain :: IsPHASEGroup phaseGroup => phaseGroup -> IO CDouble
gain phaseGroup =
  sendMessage phaseGroup gainSelector

-- | gain
--
-- Linear gain scalar.
--
-- Note: Values are clamped to the range [0, 1]. Default value is 1.
--
-- ObjC selector: @- setGain:@
setGain :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> IO ()
setGain phaseGroup value =
  sendMessage phaseGroup setGainSelector value

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- rate@
rate :: IsPHASEGroup phaseGroup => phaseGroup -> IO CDouble
rate phaseGroup =
  sendMessage phaseGroup rateSelector

-- | rate
--
-- Linear rate scalar.
--
-- Note: Values are clamped to the range [0.25, 4]. Default value is 1.
--
-- ObjC selector: @- setRate:@
setRate :: IsPHASEGroup phaseGroup => phaseGroup -> CDouble -> IO ()
setRate phaseGroup value =
  sendMessage phaseGroup setRateSelector value

-- | muted
--
-- Whether or not this group is muted.
--
-- ObjC selector: @- muted@
muted :: IsPHASEGroup phaseGroup => phaseGroup -> IO Bool
muted phaseGroup =
  sendMessage phaseGroup mutedSelector

-- | soloed
--
-- Whether or not this group is soloed.
--
-- ObjC selector: @- soloed@
soloed :: IsPHASEGroup phaseGroup => phaseGroup -> IO Bool
soloed phaseGroup =
  sendMessage phaseGroup soloedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEGroup)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id PHASEGroup)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @registerWithEngine:@
registerWithEngineSelector :: Selector '[Id PHASEEngine] ()
registerWithEngineSelector = mkSelector "registerWithEngine:"

-- | @Selector@ for @unregisterFromEngine@
unregisterFromEngineSelector :: Selector '[] ()
unregisterFromEngineSelector = mkSelector "unregisterFromEngine"

-- | @Selector@ for @fadeGain:duration:curveType:@
fadeGain_duration_curveTypeSelector :: Selector '[CDouble, CDouble, PHASECurveType] ()
fadeGain_duration_curveTypeSelector = mkSelector "fadeGain:duration:curveType:"

-- | @Selector@ for @fadeRate:duration:curveType:@
fadeRate_duration_curveTypeSelector :: Selector '[CDouble, CDouble, PHASECurveType] ()
fadeRate_duration_curveTypeSelector = mkSelector "fadeRate:duration:curveType:"

-- | @Selector@ for @mute@
muteSelector :: Selector '[] ()
muteSelector = mkSelector "mute"

-- | @Selector@ for @unmute@
unmuteSelector :: Selector '[] ()
unmuteSelector = mkSelector "unmute"

-- | @Selector@ for @solo@
soloSelector :: Selector '[] ()
soloSelector = mkSelector "solo"

-- | @Selector@ for @unsolo@
unsoloSelector :: Selector '[] ()
unsoloSelector = mkSelector "unsolo"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CDouble
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector '[CDouble] ()
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CDouble
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CDouble] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @soloed@
soloedSelector :: Selector '[] Bool
soloedSelector = mkSelector "soloed"


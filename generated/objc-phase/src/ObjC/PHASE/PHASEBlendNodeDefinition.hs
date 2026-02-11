{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEBlendNodeDefinition
--
-- An object for defining a blend sound event node when building a sound event.
--
-- A blend node blends between its children based on a numeric parameter.
--
-- Generated bindings for @PHASEBlendNodeDefinition@.
module ObjC.PHASE.PHASEBlendNodeDefinition
  ( PHASEBlendNodeDefinition
  , IsPHASEBlendNodeDefinition(..)
  , init_
  , new
  , initWithBlendMetaParameterDefinition_identifier
  , initWithBlendMetaParameterDefinition
  , initDistanceBlendWithSpatialMixerDefinition_identifier
  , initDistanceBlendWithSpatialMixerDefinition
  , addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtree
  , addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtree
  , addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtree
  , addRangeWithEnvelope_subtree
  , blendParameterDefinition
  , spatialMixerDefinitionForDistance
  , initSelector
  , newSelector
  , initWithBlendMetaParameterDefinition_identifierSelector
  , initWithBlendMetaParameterDefinitionSelector
  , initDistanceBlendWithSpatialMixerDefinition_identifierSelector
  , initDistanceBlendWithSpatialMixerDefinitionSelector
  , addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtreeSelector
  , addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtreeSelector
  , addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtreeSelector
  , addRangeWithEnvelope_subtreeSelector
  , blendParameterDefinitionSelector
  , spatialMixerDefinitionForDistanceSelector

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
init_ :: IsPHASEBlendNodeDefinition phaseBlendNodeDefinition => phaseBlendNodeDefinition -> IO (Id PHASEBlendNodeDefinition)
init_ phaseBlendNodeDefinition  =
  sendMsg phaseBlendNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEBlendNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEBlendNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithBlendMetaParameterDefinition:identifier
--
-- Create a blend node definition
--
-- @blendMetaParameterDefinition@ — A metaparameter definition that wil be used to control the parameter of the blend node at runtime.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEBlendNodeDefinition object
--
-- ObjC selector: @- initWithBlendMetaParameterDefinition:identifier:@
initWithBlendMetaParameterDefinition_identifier :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASENumberMetaParameterDefinition blendMetaParameterDefinition, IsNSString identifier) => phaseBlendNodeDefinition -> blendMetaParameterDefinition -> identifier -> IO (Id PHASEBlendNodeDefinition)
initWithBlendMetaParameterDefinition_identifier phaseBlendNodeDefinition  blendMetaParameterDefinition identifier =
withObjCPtr blendMetaParameterDefinition $ \raw_blendMetaParameterDefinition ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseBlendNodeDefinition (mkSelector "initWithBlendMetaParameterDefinition:identifier:") (retPtr retVoid) [argPtr (castPtr raw_blendMetaParameterDefinition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | initWithBlendMetaParameterDefinition
--
-- Create a blend node definition
--
-- @blendMetaParameterDefinition@ — A metaparameter definition that wil be used to control the parameter of the blend node at runtime.
--
-- Returns: A new PHASEBlendNodeDefinition object
--
-- ObjC selector: @- initWithBlendMetaParameterDefinition:@
initWithBlendMetaParameterDefinition :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASENumberMetaParameterDefinition blendMetaParameterDefinition) => phaseBlendNodeDefinition -> blendMetaParameterDefinition -> IO (Id PHASEBlendNodeDefinition)
initWithBlendMetaParameterDefinition phaseBlendNodeDefinition  blendMetaParameterDefinition =
withObjCPtr blendMetaParameterDefinition $ \raw_blendMetaParameterDefinition ->
    sendMsg phaseBlendNodeDefinition (mkSelector "initWithBlendMetaParameterDefinition:") (retPtr retVoid) [argPtr (castPtr raw_blendMetaParameterDefinition :: Ptr ())] >>= ownedObject . castPtr

-- | initDistanceBlendWithSpatialMixerDefinition:identifier
--
-- Create a blend node definition
--
-- @spatialMixerDefinition@ — A PHASESpatialMixerDefinition that will bind the blend parameter to the distance between the source and listener.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEBlendNodeDefinition object
--
-- ObjC selector: @- initDistanceBlendWithSpatialMixerDefinition:identifier:@
initDistanceBlendWithSpatialMixerDefinition_identifier :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASESpatialMixerDefinition spatialMixerDefinition, IsNSString identifier) => phaseBlendNodeDefinition -> spatialMixerDefinition -> identifier -> IO (Id PHASEBlendNodeDefinition)
initDistanceBlendWithSpatialMixerDefinition_identifier phaseBlendNodeDefinition  spatialMixerDefinition identifier =
withObjCPtr spatialMixerDefinition $ \raw_spatialMixerDefinition ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg phaseBlendNodeDefinition (mkSelector "initDistanceBlendWithSpatialMixerDefinition:identifier:") (retPtr retVoid) [argPtr (castPtr raw_spatialMixerDefinition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | initDistanceBlendWithSpatialMixerDefinition
--
-- Create a blend node definition
--
-- @spatialMixerDefinition@ — A PHASESpatialMixerDefinition that will bind the blend parameter to the distance between the source and listener.
--
-- Returns: A new PHASEBlendNodeDefinition object
--
-- ObjC selector: @- initDistanceBlendWithSpatialMixerDefinition:@
initDistanceBlendWithSpatialMixerDefinition :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASESpatialMixerDefinition spatialMixerDefinition) => phaseBlendNodeDefinition -> spatialMixerDefinition -> IO (Id PHASEBlendNodeDefinition)
initDistanceBlendWithSpatialMixerDefinition phaseBlendNodeDefinition  spatialMixerDefinition =
withObjCPtr spatialMixerDefinition $ \raw_spatialMixerDefinition ->
    sendMsg phaseBlendNodeDefinition (mkSelector "initDistanceBlendWithSpatialMixerDefinition:") (retPtr retVoid) [argPtr (castPtr raw_spatialMixerDefinition :: Ptr ())] >>= ownedObject . castPtr

-- | addRangeForInputValuesBelow:fullGainAtValue:fadeCurveType:subtree
--
-- Create a blend range that is active for all values below a given threshold.
--
-- @value@ — The value for which the range will be active if the blend node's input value is below this threshold.
--
-- @fullGainAtValue@ — This value defines a threshold for which a fade curve will be applied to the gain, when the input value is between "value" and "fullGainAtValue".
--
-- @fadeCurveType@ — A curve type that defines which kind of fade curve to apply.
--
-- @subtree@ — A PHASESoundEventNodeDefinition subtree that will be active for this range.
--
-- ObjC selector: @- addRangeForInputValuesBelow:fullGainAtValue:fadeCurveType:subtree:@
addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtree :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASESoundEventNodeDefinition subtree) => phaseBlendNodeDefinition -> CDouble -> CDouble -> PHASECurveType -> subtree -> IO ()
addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtree phaseBlendNodeDefinition  value fullGainAtValue fadeCurveType subtree =
withObjCPtr subtree $ \raw_subtree ->
    sendMsg phaseBlendNodeDefinition (mkSelector "addRangeForInputValuesBelow:fullGainAtValue:fadeCurveType:subtree:") retVoid [argCDouble (fromIntegral value), argCDouble (fromIntegral fullGainAtValue), argCLong (coerce fadeCurveType), argPtr (castPtr raw_subtree :: Ptr ())]

-- | addRangeForInputValuesBetween:highValue:fullGainAtLowValue:fullGainAtHighValue:lowFadeCurveType:highFadeCurveType:subtree
--
-- Create a blend range that is active for all input values between lowValue and highValue
--
-- @lowValue@ — The lower bound for which this range is active.
--
-- @highValue@ — The upper bound for which this range is active.
--
-- @fullGainAtLowValue@ — The threshold for which a fade curve defined by lowFadeCurveType will be applied to the gain when the input value is between lowValue and fullGainAtLowValue
--
-- @fullGainAtHighValue@ — The threshold for which a fade curve defined by highFadeCurveType will be applied to the gain when the input value is between highValue and fullGainAtHighValue
--
-- @lowFadeCurveType@ — A curve type that defines which kind of fade curve to apply for the low fade range.
--
-- @highFadeCurveType@ — A curve type that defines which kind of fade curve to apply for the high fade range..
--
-- @subtree@ — A PHASESoundEventNodeDefinition subtree that will be active for this range.
--
-- ObjC selector: @- addRangeForInputValuesBetween:highValue:fullGainAtLowValue:fullGainAtHighValue:lowFadeCurveType:highFadeCurveType:subtree:@
addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtree :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASESoundEventNodeDefinition subtree) => phaseBlendNodeDefinition -> CDouble -> CDouble -> CDouble -> CDouble -> PHASECurveType -> PHASECurveType -> subtree -> IO ()
addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtree phaseBlendNodeDefinition  lowValue highValue fullGainAtLowValue fullGainAtHighValue lowFadeCurveType highFadeCurveType subtree =
withObjCPtr subtree $ \raw_subtree ->
    sendMsg phaseBlendNodeDefinition (mkSelector "addRangeForInputValuesBetween:highValue:fullGainAtLowValue:fullGainAtHighValue:lowFadeCurveType:highFadeCurveType:subtree:") retVoid [argCDouble (fromIntegral lowValue), argCDouble (fromIntegral highValue), argCDouble (fromIntegral fullGainAtLowValue), argCDouble (fromIntegral fullGainAtHighValue), argCLong (coerce lowFadeCurveType), argCLong (coerce highFadeCurveType), argPtr (castPtr raw_subtree :: Ptr ())]

-- | addRangeForInputValuesAbove:fullGainAtValue:fadeCurveType:subtree
--
-- Create a blend range that is active for all values above a given threshold.
--
-- @value@ — The value for which the range will be active if the blend node's input value is above this threshold.
--
-- @fullGainAtValue@ — This value defines a threshold for which a fade curve will be applied to the gain, when the input value is between "value" and "fullGainAtValue".
--
-- @fadeCurveType@ — A curve type that defines which kind of fade curve to apply.
--
-- @subtree@ — A PHASESoundEventNodeDefinition subtree that will be active for this range.
--
-- ObjC selector: @- addRangeForInputValuesAbove:fullGainAtValue:fadeCurveType:subtree:@
addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtree :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASESoundEventNodeDefinition subtree) => phaseBlendNodeDefinition -> CDouble -> CDouble -> PHASECurveType -> subtree -> IO ()
addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtree phaseBlendNodeDefinition  value fullGainAtValue fadeCurveType subtree =
withObjCPtr subtree $ \raw_subtree ->
    sendMsg phaseBlendNodeDefinition (mkSelector "addRangeForInputValuesAbove:fullGainAtValue:fadeCurveType:subtree:") retVoid [argCDouble (fromIntegral value), argCDouble (fromIntegral fullGainAtValue), argCLong (coerce fadeCurveType), argPtr (castPtr raw_subtree :: Ptr ())]

-- | addRangeWithEnvelope:subtree
--
-- Create a blend range defined by a PHASEEnvelope object.
--
-- @envelope@ — The PHASEEnvelope object that defines the output gain for a range.
--
-- @subtree@ — A PHASESoundEventNodeDefinition subtree that will be active for this range.
--
-- ObjC selector: @- addRangeWithEnvelope:subtree:@
addRangeWithEnvelope_subtree :: (IsPHASEBlendNodeDefinition phaseBlendNodeDefinition, IsPHASEEnvelope envelope, IsPHASESoundEventNodeDefinition subtree) => phaseBlendNodeDefinition -> envelope -> subtree -> IO ()
addRangeWithEnvelope_subtree phaseBlendNodeDefinition  envelope subtree =
withObjCPtr envelope $ \raw_envelope ->
  withObjCPtr subtree $ \raw_subtree ->
      sendMsg phaseBlendNodeDefinition (mkSelector "addRangeWithEnvelope:subtree:") retVoid [argPtr (castPtr raw_envelope :: Ptr ()), argPtr (castPtr raw_subtree :: Ptr ())]

-- | blendParameterDefinition
--
-- The readonly property that returns the PHASENumberMetaParameterDefinition this blend node was created with and assigned to.
--
-- ObjC selector: @- blendParameterDefinition@
blendParameterDefinition :: IsPHASEBlendNodeDefinition phaseBlendNodeDefinition => phaseBlendNodeDefinition -> IO (Id PHASENumberMetaParameterDefinition)
blendParameterDefinition phaseBlendNodeDefinition  =
  sendMsg phaseBlendNodeDefinition (mkSelector "blendParameterDefinition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | distanceSpatialMixerDefinition
--
-- The readonly property that returns the PHASESpatialMixerDefinition this blend node was created with and assigned to.
--
-- ObjC selector: @- spatialMixerDefinitionForDistance@
spatialMixerDefinitionForDistance :: IsPHASEBlendNodeDefinition phaseBlendNodeDefinition => phaseBlendNodeDefinition -> IO (Id PHASESpatialMixerDefinition)
spatialMixerDefinitionForDistance phaseBlendNodeDefinition  =
  sendMsg phaseBlendNodeDefinition (mkSelector "spatialMixerDefinitionForDistance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithBlendMetaParameterDefinition:identifier:@
initWithBlendMetaParameterDefinition_identifierSelector :: Selector
initWithBlendMetaParameterDefinition_identifierSelector = mkSelector "initWithBlendMetaParameterDefinition:identifier:"

-- | @Selector@ for @initWithBlendMetaParameterDefinition:@
initWithBlendMetaParameterDefinitionSelector :: Selector
initWithBlendMetaParameterDefinitionSelector = mkSelector "initWithBlendMetaParameterDefinition:"

-- | @Selector@ for @initDistanceBlendWithSpatialMixerDefinition:identifier:@
initDistanceBlendWithSpatialMixerDefinition_identifierSelector :: Selector
initDistanceBlendWithSpatialMixerDefinition_identifierSelector = mkSelector "initDistanceBlendWithSpatialMixerDefinition:identifier:"

-- | @Selector@ for @initDistanceBlendWithSpatialMixerDefinition:@
initDistanceBlendWithSpatialMixerDefinitionSelector :: Selector
initDistanceBlendWithSpatialMixerDefinitionSelector = mkSelector "initDistanceBlendWithSpatialMixerDefinition:"

-- | @Selector@ for @addRangeForInputValuesBelow:fullGainAtValue:fadeCurveType:subtree:@
addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtreeSelector :: Selector
addRangeForInputValuesBelow_fullGainAtValue_fadeCurveType_subtreeSelector = mkSelector "addRangeForInputValuesBelow:fullGainAtValue:fadeCurveType:subtree:"

-- | @Selector@ for @addRangeForInputValuesBetween:highValue:fullGainAtLowValue:fullGainAtHighValue:lowFadeCurveType:highFadeCurveType:subtree:@
addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtreeSelector :: Selector
addRangeForInputValuesBetween_highValue_fullGainAtLowValue_fullGainAtHighValue_lowFadeCurveType_highFadeCurveType_subtreeSelector = mkSelector "addRangeForInputValuesBetween:highValue:fullGainAtLowValue:fullGainAtHighValue:lowFadeCurveType:highFadeCurveType:subtree:"

-- | @Selector@ for @addRangeForInputValuesAbove:fullGainAtValue:fadeCurveType:subtree:@
addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtreeSelector :: Selector
addRangeForInputValuesAbove_fullGainAtValue_fadeCurveType_subtreeSelector = mkSelector "addRangeForInputValuesAbove:fullGainAtValue:fadeCurveType:subtree:"

-- | @Selector@ for @addRangeWithEnvelope:subtree:@
addRangeWithEnvelope_subtreeSelector :: Selector
addRangeWithEnvelope_subtreeSelector = mkSelector "addRangeWithEnvelope:subtree:"

-- | @Selector@ for @blendParameterDefinition@
blendParameterDefinitionSelector :: Selector
blendParameterDefinitionSelector = mkSelector "blendParameterDefinition"

-- | @Selector@ for @spatialMixerDefinitionForDistance@
spatialMixerDefinitionForDistanceSelector :: Selector
spatialMixerDefinitionForDistanceSelector = mkSelector "spatialMixerDefinitionForDistance"


{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A description of a model containing input, output, and state feature descriptions, optionally outputted features with special meaning and metadata.
--
-- Generated bindings for @MLModelDescription@.
module ObjC.CoreML.MLModelDescription
  ( MLModelDescription
  , IsMLModelDescription(..)
  , inputDescriptionsByName
  , outputDescriptionsByName
  , stateDescriptionsByName
  , predictedFeatureName
  , predictedProbabilitiesName
  , metadata
  , classLabels
  , parameterDescriptionsByKey
  , isUpdatable
  , trainingInputDescriptionsByName
  , inputDescriptionsByNameSelector
  , outputDescriptionsByNameSelector
  , stateDescriptionsByNameSelector
  , predictedFeatureNameSelector
  , predictedProbabilitiesNameSelector
  , metadataSelector
  , classLabelsSelector
  , parameterDescriptionsByKeySelector
  , isUpdatableSelector
  , trainingInputDescriptionsByNameSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Description of the inputs to the model
--
-- ObjC selector: @- inputDescriptionsByName@
inputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
inputDescriptionsByName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "inputDescriptionsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Description of the outputs from the model
--
-- ObjC selector: @- outputDescriptionsByName@
outputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
outputDescriptionsByName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "outputDescriptionsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Description of the state features.
--
-- ObjC selector: @- stateDescriptionsByName@
stateDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
stateDescriptionsByName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "stateDescriptionsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of the primary target / predicted output feature in the output descriptions
--
-- ObjC selector: @- predictedFeatureName@
predictedFeatureName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSString)
predictedFeatureName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "predictedFeatureName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Key for all predicted probabilities stored as a MLFeatureTypeDictionary in the output descriptions
--
-- ObjC selector: @- predictedProbabilitiesName@
predictedProbabilitiesName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSString)
predictedProbabilitiesName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "predictedProbabilitiesName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional metadata describing the model
--
-- ObjC selector: @- metadata@
metadata :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
metadata mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array to map a class index to the corresponding label, which is either Number or String.
--
-- The property is populated from the classLabels entry specified in the model's protobuf message. When the model is a pipeline, which contains one or more sub models, the property value is calculated as follows.
--
-- 1. If the pipeline model's proto message specifies predictedFeatureName parameter, use classLabels property value of the sub model with the output feature with the name.
--
-- 2. Otherwise, if the pipeline model has only one sub model with non-nil classLabels property, use the property value.
--
-- 3. Otherwise, the property is nil.
--
-- ObjC selector: @- classLabels@
classLabels :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSArray)
classLabels mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "classLabels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parameterDescriptionsByKey@
parameterDescriptionsByKey :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
parameterDescriptionsByKey mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "parameterDescriptionsByKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isUpdatable@
isUpdatable :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO Bool
isUpdatable mlModelDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlModelDescription (mkSelector "isUpdatable") retCULong []

-- | @- trainingInputDescriptionsByName@
trainingInputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
trainingInputDescriptionsByName mlModelDescription  =
  sendMsg mlModelDescription (mkSelector "trainingInputDescriptionsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputDescriptionsByName@
inputDescriptionsByNameSelector :: Selector
inputDescriptionsByNameSelector = mkSelector "inputDescriptionsByName"

-- | @Selector@ for @outputDescriptionsByName@
outputDescriptionsByNameSelector :: Selector
outputDescriptionsByNameSelector = mkSelector "outputDescriptionsByName"

-- | @Selector@ for @stateDescriptionsByName@
stateDescriptionsByNameSelector :: Selector
stateDescriptionsByNameSelector = mkSelector "stateDescriptionsByName"

-- | @Selector@ for @predictedFeatureName@
predictedFeatureNameSelector :: Selector
predictedFeatureNameSelector = mkSelector "predictedFeatureName"

-- | @Selector@ for @predictedProbabilitiesName@
predictedProbabilitiesNameSelector :: Selector
predictedProbabilitiesNameSelector = mkSelector "predictedProbabilitiesName"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @classLabels@
classLabelsSelector :: Selector
classLabelsSelector = mkSelector "classLabels"

-- | @Selector@ for @parameterDescriptionsByKey@
parameterDescriptionsByKeySelector :: Selector
parameterDescriptionsByKeySelector = mkSelector "parameterDescriptionsByKey"

-- | @Selector@ for @isUpdatable@
isUpdatableSelector :: Selector
isUpdatableSelector = mkSelector "isUpdatable"

-- | @Selector@ for @trainingInputDescriptionsByName@
trainingInputDescriptionsByNameSelector :: Selector
trainingInputDescriptionsByNameSelector = mkSelector "trainingInputDescriptionsByName"


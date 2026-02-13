{-# LANGUAGE DataKinds #-}
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
  , classLabelsSelector
  , inputDescriptionsByNameSelector
  , isUpdatableSelector
  , metadataSelector
  , outputDescriptionsByNameSelector
  , parameterDescriptionsByKeySelector
  , predictedFeatureNameSelector
  , predictedProbabilitiesNameSelector
  , stateDescriptionsByNameSelector
  , trainingInputDescriptionsByNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Description of the inputs to the model
--
-- ObjC selector: @- inputDescriptionsByName@
inputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
inputDescriptionsByName mlModelDescription =
  sendMessage mlModelDescription inputDescriptionsByNameSelector

-- | Description of the outputs from the model
--
-- ObjC selector: @- outputDescriptionsByName@
outputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
outputDescriptionsByName mlModelDescription =
  sendMessage mlModelDescription outputDescriptionsByNameSelector

-- | Description of the state features.
--
-- ObjC selector: @- stateDescriptionsByName@
stateDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
stateDescriptionsByName mlModelDescription =
  sendMessage mlModelDescription stateDescriptionsByNameSelector

-- | Name of the primary target / predicted output feature in the output descriptions
--
-- ObjC selector: @- predictedFeatureName@
predictedFeatureName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSString)
predictedFeatureName mlModelDescription =
  sendMessage mlModelDescription predictedFeatureNameSelector

-- | Key for all predicted probabilities stored as a MLFeatureTypeDictionary in the output descriptions
--
-- ObjC selector: @- predictedProbabilitiesName@
predictedProbabilitiesName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSString)
predictedProbabilitiesName mlModelDescription =
  sendMessage mlModelDescription predictedProbabilitiesNameSelector

-- | Optional metadata describing the model
--
-- ObjC selector: @- metadata@
metadata :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
metadata mlModelDescription =
  sendMessage mlModelDescription metadataSelector

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
classLabels mlModelDescription =
  sendMessage mlModelDescription classLabelsSelector

-- | @- parameterDescriptionsByKey@
parameterDescriptionsByKey :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
parameterDescriptionsByKey mlModelDescription =
  sendMessage mlModelDescription parameterDescriptionsByKeySelector

-- | @- isUpdatable@
isUpdatable :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO Bool
isUpdatable mlModelDescription =
  sendMessage mlModelDescription isUpdatableSelector

-- | @- trainingInputDescriptionsByName@
trainingInputDescriptionsByName :: IsMLModelDescription mlModelDescription => mlModelDescription -> IO (Id NSDictionary)
trainingInputDescriptionsByName mlModelDescription =
  sendMessage mlModelDescription trainingInputDescriptionsByNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputDescriptionsByName@
inputDescriptionsByNameSelector :: Selector '[] (Id NSDictionary)
inputDescriptionsByNameSelector = mkSelector "inputDescriptionsByName"

-- | @Selector@ for @outputDescriptionsByName@
outputDescriptionsByNameSelector :: Selector '[] (Id NSDictionary)
outputDescriptionsByNameSelector = mkSelector "outputDescriptionsByName"

-- | @Selector@ for @stateDescriptionsByName@
stateDescriptionsByNameSelector :: Selector '[] (Id NSDictionary)
stateDescriptionsByNameSelector = mkSelector "stateDescriptionsByName"

-- | @Selector@ for @predictedFeatureName@
predictedFeatureNameSelector :: Selector '[] (Id NSString)
predictedFeatureNameSelector = mkSelector "predictedFeatureName"

-- | @Selector@ for @predictedProbabilitiesName@
predictedProbabilitiesNameSelector :: Selector '[] (Id NSString)
predictedProbabilitiesNameSelector = mkSelector "predictedProbabilitiesName"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @classLabels@
classLabelsSelector :: Selector '[] (Id NSArray)
classLabelsSelector = mkSelector "classLabels"

-- | @Selector@ for @parameterDescriptionsByKey@
parameterDescriptionsByKeySelector :: Selector '[] (Id NSDictionary)
parameterDescriptionsByKeySelector = mkSelector "parameterDescriptionsByKey"

-- | @Selector@ for @isUpdatable@
isUpdatableSelector :: Selector '[] Bool
isUpdatableSelector = mkSelector "isUpdatable"

-- | @Selector@ for @trainingInputDescriptionsByName@
trainingInputDescriptionsByNameSelector :: Selector '[] (Id NSDictionary)
trainingInputDescriptionsByNameSelector = mkSelector "trainingInputDescriptionsByName"


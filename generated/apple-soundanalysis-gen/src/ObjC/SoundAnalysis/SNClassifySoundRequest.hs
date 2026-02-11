{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configure an analyzer to perform sound classification using the provided MLModel.
--
-- When a new classification result is produced, the results observer will be called with an SNClassificationResult. Audio buffers provided to SNAudioStreamAnalyzer may vary in size, and the analyzer will reblock the audio data to the block size expected by the MLModel. By default, analysis will occur on the first audio channel in the audio stream, and the analyzer will apply sample rate conversion if the provided audio does not match the sample rate required by the MLModel.
--
-- Generated bindings for @SNClassifySoundRequest@.
module ObjC.SoundAnalysis.SNClassifySoundRequest
  ( SNClassifySoundRequest
  , IsSNClassifySoundRequest(..)
  , initWithMLModel_error
  , initWithClassifierIdentifier_error
  , init_
  , new
  , overlapFactor
  , setOverlapFactor
  , windowDurationConstraint
  , knownClassifications
  , initWithMLModel_errorSelector
  , initWithClassifierIdentifier_errorSelector
  , initSelector
  , newSelector
  , overlapFactorSelector
  , setOverlapFactorSelector
  , windowDurationConstraintSelector
  , knownClassificationsSelector


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

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a sound classification request with the provided MLModel
--
-- - Parameter mlModel: The CoreML audio classification model to be used with this request
--
-- The provided model must accept audio data as input, and output a classification dictionary containing the probability of each category.
--
-- ObjC selector: @- initWithMLModel:error:@
initWithMLModel_error :: (IsSNClassifySoundRequest snClassifySoundRequest, IsMLModel mlModel, IsNSError error_) => snClassifySoundRequest -> mlModel -> error_ -> IO (Id SNClassifySoundRequest)
initWithMLModel_error snClassifySoundRequest  mlModel error_ =
  withObjCPtr mlModel $ \raw_mlModel ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg snClassifySoundRequest (mkSelector "initWithMLModel:error:") (retPtr retVoid) [argPtr (castPtr raw_mlModel :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a sound classification request with a known classifier.
--
-- - Parameters:
--
-- - classifierIdentifier: An identifier identifying the particular classifier to use for labeling sounds.
--
-- - error: An output parameter which, in the case of an error, will be populated with details about that error. Upon success, the contents of this output parameter are undefined. Please use the return value of this method to determine whether or not an error occurred before using the value assigned to this output parameter.
--
-- - Returns Upon failure, @nil@; upon success, an @SNClassifySoundRequest@ instance which can be added to an analyzer to classify sounds using a recognized classifier.
--
-- This initializer may be used to classify sounds using Apple-provided sound classifiers. Note that Apple may add new classifiers in the future, but it commits to ensuring the consistent performance of existing classifiers.
--
-- ObjC selector: @- initWithClassifierIdentifier:error:@
initWithClassifierIdentifier_error :: (IsSNClassifySoundRequest snClassifySoundRequest, IsNSString classifierIdentifier, IsNSError error_) => snClassifySoundRequest -> classifierIdentifier -> error_ -> IO (Id SNClassifySoundRequest)
initWithClassifierIdentifier_error snClassifySoundRequest  classifierIdentifier error_ =
  withObjCPtr classifierIdentifier $ \raw_classifierIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg snClassifySoundRequest (mkSelector "initWithClassifierIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_classifierIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSNClassifySoundRequest snClassifySoundRequest => snClassifySoundRequest -> IO (Id SNClassifySoundRequest)
init_ snClassifySoundRequest  =
    sendMsg snClassifySoundRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SNClassifySoundRequest)
new  =
  do
    cls' <- getRequiredClass "SNClassifySoundRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The overlap factor of the windows of audio data provided to the classifier, if the model operates on fixed audio block sizes.
--
-- When performing audio analysis on fixed audio block sizes, it is common for the analysis windows to overlap by some factor. Without overlapping the analysis windows (when the overlap factor is 0.0), a sound might be split across two analysis windows, which could negatively affect classification performance. Overlapping the analysis windows by 50% ensures each sound will fall near the center of at least one analysis window. The supported range is [0.0, 1.0), and the default value is 0.5. Increasing the overlap factor increases computational complexity, so values greater than 0.5 should be used with care.
--
-- ObjC selector: @- overlapFactor@
overlapFactor :: IsSNClassifySoundRequest snClassifySoundRequest => snClassifySoundRequest -> IO CDouble
overlapFactor snClassifySoundRequest  =
    sendMsg snClassifySoundRequest (mkSelector "overlapFactor") retCDouble []

-- | The overlap factor of the windows of audio data provided to the classifier, if the model operates on fixed audio block sizes.
--
-- When performing audio analysis on fixed audio block sizes, it is common for the analysis windows to overlap by some factor. Without overlapping the analysis windows (when the overlap factor is 0.0), a sound might be split across two analysis windows, which could negatively affect classification performance. Overlapping the analysis windows by 50% ensures each sound will fall near the center of at least one analysis window. The supported range is [0.0, 1.0), and the default value is 0.5. Increasing the overlap factor increases computational complexity, so values greater than 0.5 should be used with care.
--
-- ObjC selector: @- setOverlapFactor:@
setOverlapFactor :: IsSNClassifySoundRequest snClassifySoundRequest => snClassifySoundRequest -> CDouble -> IO ()
setOverlapFactor snClassifySoundRequest  value =
    sendMsg snClassifySoundRequest (mkSelector "setOverlapFactor:") retVoid [argCDouble value]

-- | The constraints governing permitted analysis window durations.
--
-- The analysis window duration is controlled using the @windowDuration@ property. If an analysis window duration is selected which does not meet the necessary constraints, it will automatically be adjusted to meet these constraints (see @windowDuration@ for more information regarding how this adjustment will be applied).
--
-- ObjC selector: @- windowDurationConstraint@
windowDurationConstraint :: IsSNClassifySoundRequest snClassifySoundRequest => snClassifySoundRequest -> IO (Id SNTimeDurationConstraint)
windowDurationConstraint snClassifySoundRequest  =
    sendMsg snClassifySoundRequest (mkSelector "windowDurationConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Lists all labels that can be produced by this request.
--
-- - Returns: An array of strings containing all sound identifiers which can be produced by this request.
--
-- ObjC selector: @- knownClassifications@
knownClassifications :: IsSNClassifySoundRequest snClassifySoundRequest => snClassifySoundRequest -> IO (Id NSArray)
knownClassifications snClassifySoundRequest  =
    sendMsg snClassifySoundRequest (mkSelector "knownClassifications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMLModel:error:@
initWithMLModel_errorSelector :: Selector
initWithMLModel_errorSelector = mkSelector "initWithMLModel:error:"

-- | @Selector@ for @initWithClassifierIdentifier:error:@
initWithClassifierIdentifier_errorSelector :: Selector
initWithClassifierIdentifier_errorSelector = mkSelector "initWithClassifierIdentifier:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @overlapFactor@
overlapFactorSelector :: Selector
overlapFactorSelector = mkSelector "overlapFactor"

-- | @Selector@ for @setOverlapFactor:@
setOverlapFactorSelector :: Selector
setOverlapFactorSelector = mkSelector "setOverlapFactor:"

-- | @Selector@ for @windowDurationConstraint@
windowDurationConstraintSelector :: Selector
windowDurationConstraintSelector = mkSelector "windowDurationConstraint"

-- | @Selector@ for @knownClassifications@
knownClassificationsSelector :: Selector
knownClassificationsSelector = mkSelector "knownClassifications"


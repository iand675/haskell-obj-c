{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLModel@.
module ObjC.NaturalLanguage.NLModel
  ( NLModel
  , IsNLModel(..)
  , modelWithContentsOfURL_error
  , modelWithMLModel_error
  , predictedLabelForString
  , predictedLabelsForTokens
  , predictedLabelHypothesesForString_maximumCount
  , predictedLabelHypothesesForTokens_maximumCount
  , configuration
  , configurationSelector
  , modelWithContentsOfURL_errorSelector
  , modelWithMLModel_errorSelector
  , predictedLabelForStringSelector
  , predictedLabelHypothesesForString_maximumCountSelector
  , predictedLabelHypothesesForTokens_maximumCountSelector
  , predictedLabelsForTokensSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ modelWithContentsOfURL:error:@
modelWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLModel)
modelWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLModel"
    sendClassMessage cls' modelWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ modelWithMLModel:error:@
modelWithMLModel_error :: (IsMLModel mlModel, IsNSError error_) => mlModel -> error_ -> IO (Id NLModel)
modelWithMLModel_error mlModel error_ =
  do
    cls' <- getRequiredClass "NLModel"
    sendClassMessage cls' modelWithMLModel_errorSelector (toMLModel mlModel) (toNSError error_)

-- | @- predictedLabelForString:@
predictedLabelForString :: (IsNLModel nlModel, IsNSString string) => nlModel -> string -> IO (Id NSString)
predictedLabelForString nlModel string =
  sendMessage nlModel predictedLabelForStringSelector (toNSString string)

-- | @- predictedLabelsForTokens:@
predictedLabelsForTokens :: (IsNLModel nlModel, IsNSArray tokens) => nlModel -> tokens -> IO (Id NSArray)
predictedLabelsForTokens nlModel tokens =
  sendMessage nlModel predictedLabelsForTokensSelector (toNSArray tokens)

-- | @- predictedLabelHypothesesForString:maximumCount:@
predictedLabelHypothesesForString_maximumCount :: (IsNLModel nlModel, IsNSString string) => nlModel -> string -> CULong -> IO (Id NSDictionary)
predictedLabelHypothesesForString_maximumCount nlModel string maximumCount =
  sendMessage nlModel predictedLabelHypothesesForString_maximumCountSelector (toNSString string) maximumCount

-- | @- predictedLabelHypothesesForTokens:maximumCount:@
predictedLabelHypothesesForTokens_maximumCount :: (IsNLModel nlModel, IsNSArray tokens) => nlModel -> tokens -> CULong -> IO (Id NSArray)
predictedLabelHypothesesForTokens_maximumCount nlModel tokens maximumCount =
  sendMessage nlModel predictedLabelHypothesesForTokens_maximumCountSelector (toNSArray tokens) maximumCount

-- | @- configuration@
configuration :: IsNLModel nlModel => nlModel -> IO (Id NLModelConfiguration)
configuration nlModel =
  sendMessage nlModel configurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modelWithContentsOfURL:error:@
modelWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NLModel)
modelWithContentsOfURL_errorSelector = mkSelector "modelWithContentsOfURL:error:"

-- | @Selector@ for @modelWithMLModel:error:@
modelWithMLModel_errorSelector :: Selector '[Id MLModel, Id NSError] (Id NLModel)
modelWithMLModel_errorSelector = mkSelector "modelWithMLModel:error:"

-- | @Selector@ for @predictedLabelForString:@
predictedLabelForStringSelector :: Selector '[Id NSString] (Id NSString)
predictedLabelForStringSelector = mkSelector "predictedLabelForString:"

-- | @Selector@ for @predictedLabelsForTokens:@
predictedLabelsForTokensSelector :: Selector '[Id NSArray] (Id NSArray)
predictedLabelsForTokensSelector = mkSelector "predictedLabelsForTokens:"

-- | @Selector@ for @predictedLabelHypothesesForString:maximumCount:@
predictedLabelHypothesesForString_maximumCountSelector :: Selector '[Id NSString, CULong] (Id NSDictionary)
predictedLabelHypothesesForString_maximumCountSelector = mkSelector "predictedLabelHypothesesForString:maximumCount:"

-- | @Selector@ for @predictedLabelHypothesesForTokens:maximumCount:@
predictedLabelHypothesesForTokens_maximumCountSelector :: Selector '[Id NSArray, CULong] (Id NSArray)
predictedLabelHypothesesForTokens_maximumCountSelector = mkSelector "predictedLabelHypothesesForTokens:maximumCount:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id NLModelConfiguration)
configurationSelector = mkSelector "configuration"


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
  , modelWithContentsOfURL_errorSelector
  , modelWithMLModel_errorSelector
  , predictedLabelForStringSelector
  , predictedLabelsForTokensSelector
  , predictedLabelHypothesesForString_maximumCountSelector
  , predictedLabelHypothesesForTokens_maximumCountSelector
  , configurationSelector


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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ modelWithContentsOfURL:error:@
modelWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLModel)
modelWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLModel"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "modelWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ modelWithMLModel:error:@
modelWithMLModel_error :: (IsMLModel mlModel, IsNSError error_) => mlModel -> error_ -> IO (Id NLModel)
modelWithMLModel_error mlModel error_ =
  do
    cls' <- getRequiredClass "NLModel"
    withObjCPtr mlModel $ \raw_mlModel ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "modelWithMLModel:error:") (retPtr retVoid) [argPtr (castPtr raw_mlModel :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- predictedLabelForString:@
predictedLabelForString :: (IsNLModel nlModel, IsNSString string) => nlModel -> string -> IO (Id NSString)
predictedLabelForString nlModel  string =
withObjCPtr string $ \raw_string ->
    sendMsg nlModel (mkSelector "predictedLabelForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- predictedLabelsForTokens:@
predictedLabelsForTokens :: (IsNLModel nlModel, IsNSArray tokens) => nlModel -> tokens -> IO (Id NSArray)
predictedLabelsForTokens nlModel  tokens =
withObjCPtr tokens $ \raw_tokens ->
    sendMsg nlModel (mkSelector "predictedLabelsForTokens:") (retPtr retVoid) [argPtr (castPtr raw_tokens :: Ptr ())] >>= retainedObject . castPtr

-- | @- predictedLabelHypothesesForString:maximumCount:@
predictedLabelHypothesesForString_maximumCount :: (IsNLModel nlModel, IsNSString string) => nlModel -> string -> CULong -> IO (Id NSDictionary)
predictedLabelHypothesesForString_maximumCount nlModel  string maximumCount =
withObjCPtr string $ \raw_string ->
    sendMsg nlModel (mkSelector "predictedLabelHypothesesForString:maximumCount:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral maximumCount)] >>= retainedObject . castPtr

-- | @- predictedLabelHypothesesForTokens:maximumCount:@
predictedLabelHypothesesForTokens_maximumCount :: (IsNLModel nlModel, IsNSArray tokens) => nlModel -> tokens -> CULong -> IO (Id NSArray)
predictedLabelHypothesesForTokens_maximumCount nlModel  tokens maximumCount =
withObjCPtr tokens $ \raw_tokens ->
    sendMsg nlModel (mkSelector "predictedLabelHypothesesForTokens:maximumCount:") (retPtr retVoid) [argPtr (castPtr raw_tokens :: Ptr ()), argCULong (fromIntegral maximumCount)] >>= retainedObject . castPtr

-- | @- configuration@
configuration :: IsNLModel nlModel => nlModel -> IO (Id NLModelConfiguration)
configuration nlModel  =
  sendMsg nlModel (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modelWithContentsOfURL:error:@
modelWithContentsOfURL_errorSelector :: Selector
modelWithContentsOfURL_errorSelector = mkSelector "modelWithContentsOfURL:error:"

-- | @Selector@ for @modelWithMLModel:error:@
modelWithMLModel_errorSelector :: Selector
modelWithMLModel_errorSelector = mkSelector "modelWithMLModel:error:"

-- | @Selector@ for @predictedLabelForString:@
predictedLabelForStringSelector :: Selector
predictedLabelForStringSelector = mkSelector "predictedLabelForString:"

-- | @Selector@ for @predictedLabelsForTokens:@
predictedLabelsForTokensSelector :: Selector
predictedLabelsForTokensSelector = mkSelector "predictedLabelsForTokens:"

-- | @Selector@ for @predictedLabelHypothesesForString:maximumCount:@
predictedLabelHypothesesForString_maximumCountSelector :: Selector
predictedLabelHypothesesForString_maximumCountSelector = mkSelector "predictedLabelHypothesesForString:maximumCount:"

-- | @Selector@ for @predictedLabelHypothesesForTokens:maximumCount:@
predictedLabelHypothesesForTokens_maximumCountSelector :: Selector
predictedLabelHypothesesForTokens_maximumCountSelector = mkSelector "predictedLabelHypothesesForTokens:maximumCount:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"


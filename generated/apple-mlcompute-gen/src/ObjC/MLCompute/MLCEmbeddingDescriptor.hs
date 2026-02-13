{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCEmbeddingDescriptor
--
-- The MLCEmbeddingDescriptor specifies an embedding layer descriptor
--
-- Generated bindings for @MLCEmbeddingDescriptor@.
module ObjC.MLCompute.MLCEmbeddingDescriptor
  ( MLCEmbeddingDescriptor
  , IsMLCEmbeddingDescriptor(..)
  , new
  , init_
  , descriptorWithEmbeddingCount_embeddingDimension
  , descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequency
  , embeddingCount
  , embeddingDimension
  , paddingIndex
  , maximumNorm
  , pNorm
  , scalesGradientByFrequency
  , descriptorWithEmbeddingCount_embeddingDimensionSelector
  , descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector
  , embeddingCountSelector
  , embeddingDimensionSelector
  , initSelector
  , maximumNormSelector
  , newSelector
  , pNormSelector
  , paddingIndexSelector
  , scalesGradientByFrequencySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCEmbeddingDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id MLCEmbeddingDescriptor)
init_ mlcEmbeddingDescriptor =
  sendOwnedMessage mlcEmbeddingDescriptor initSelector

-- | @+ descriptorWithEmbeddingCount:embeddingDimension:@
descriptorWithEmbeddingCount_embeddingDimension :: (IsNSNumber embeddingCount, IsNSNumber embeddingDimension) => embeddingCount -> embeddingDimension -> IO (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimension embeddingCount embeddingDimension =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    sendClassMessage cls' descriptorWithEmbeddingCount_embeddingDimensionSelector (toNSNumber embeddingCount) (toNSNumber embeddingDimension)

-- | @+ descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:@
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequency :: (IsNSNumber embeddingCount, IsNSNumber embeddingDimension, IsNSNumber paddingIndex, IsNSNumber maximumNorm, IsNSNumber pNorm) => embeddingCount -> embeddingDimension -> paddingIndex -> maximumNorm -> pNorm -> Bool -> IO (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequency embeddingCount embeddingDimension paddingIndex maximumNorm pNorm scalesGradientByFrequency =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    sendClassMessage cls' descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector (toNSNumber embeddingCount) (toNSNumber embeddingDimension) (toNSNumber paddingIndex) (toNSNumber maximumNorm) (toNSNumber pNorm) scalesGradientByFrequency

-- | embeddingCount
--
-- The size of the dictionary
--
-- ObjC selector: @- embeddingCount@
embeddingCount :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
embeddingCount mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor embeddingCountSelector

-- | embeddingDimension
--
-- The dimension of embedding vectors
--
-- ObjC selector: @- embeddingDimension@
embeddingDimension :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
embeddingDimension mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor embeddingDimensionSelector

-- | paddingIndex
--
-- If set, the embedding vector at paddingIndex is initialized with zero and will not be updated in gradient pass, Default=nil
--
-- ObjC selector: @- paddingIndex@
paddingIndex :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
paddingIndex mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor paddingIndexSelector

-- | maximumNorm
--
-- A float, if set, in the forward pass only, the selected embedding vectors will be re-normalized to have an Lp norm of less than maximumNorm in the dictionary, Default=nil
--
-- ObjC selector: @- maximumNorm@
maximumNorm :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
maximumNorm mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor maximumNormSelector

-- | pNorm
--
-- A float, the p of the Lp norm, can be set to infinity norm by [NSNumber numberWithFloat:INFINITY]. Default=2.0
--
-- ObjC selector: @- pNorm@
pNorm :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
pNorm mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor pNormSelector

-- | scalesGradientByFrequency
--
-- If set, the gradients are scaled by the inverse of the frequency of the words in batch before the weight update. Default=NO
--
-- ObjC selector: @- scalesGradientByFrequency@
scalesGradientByFrequency :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO Bool
scalesGradientByFrequency mlcEmbeddingDescriptor =
  sendMessage mlcEmbeddingDescriptor scalesGradientByFrequencySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCEmbeddingDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCEmbeddingDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithEmbeddingCount:embeddingDimension:@
descriptorWithEmbeddingCount_embeddingDimensionSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimensionSelector = mkSelector "descriptorWithEmbeddingCount:embeddingDimension:"

-- | @Selector@ for @descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:@
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Id NSNumber, Bool] (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector = mkSelector "descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:"

-- | @Selector@ for @embeddingCount@
embeddingCountSelector :: Selector '[] (Id NSNumber)
embeddingCountSelector = mkSelector "embeddingCount"

-- | @Selector@ for @embeddingDimension@
embeddingDimensionSelector :: Selector '[] (Id NSNumber)
embeddingDimensionSelector = mkSelector "embeddingDimension"

-- | @Selector@ for @paddingIndex@
paddingIndexSelector :: Selector '[] (Id NSNumber)
paddingIndexSelector = mkSelector "paddingIndex"

-- | @Selector@ for @maximumNorm@
maximumNormSelector :: Selector '[] (Id NSNumber)
maximumNormSelector = mkSelector "maximumNorm"

-- | @Selector@ for @pNorm@
pNormSelector :: Selector '[] (Id NSNumber)
pNormSelector = mkSelector "pNorm"

-- | @Selector@ for @scalesGradientByFrequency@
scalesGradientByFrequencySelector :: Selector '[] Bool
scalesGradientByFrequencySelector = mkSelector "scalesGradientByFrequency"


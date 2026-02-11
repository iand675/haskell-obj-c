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
  , newSelector
  , initSelector
  , descriptorWithEmbeddingCount_embeddingDimensionSelector
  , descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector
  , embeddingCountSelector
  , embeddingDimensionSelector
  , paddingIndexSelector
  , maximumNormSelector
  , pNormSelector
  , scalesGradientByFrequencySelector


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

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCEmbeddingDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id MLCEmbeddingDescriptor)
init_ mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ descriptorWithEmbeddingCount:embeddingDimension:@
descriptorWithEmbeddingCount_embeddingDimension :: (IsNSNumber embeddingCount, IsNSNumber embeddingDimension) => embeddingCount -> embeddingDimension -> IO (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimension embeddingCount embeddingDimension =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    withObjCPtr embeddingCount $ \raw_embeddingCount ->
      withObjCPtr embeddingDimension $ \raw_embeddingDimension ->
        sendClassMsg cls' (mkSelector "descriptorWithEmbeddingCount:embeddingDimension:") (retPtr retVoid) [argPtr (castPtr raw_embeddingCount :: Ptr ()), argPtr (castPtr raw_embeddingDimension :: Ptr ())] >>= retainedObject . castPtr

-- | @+ descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:@
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequency :: (IsNSNumber embeddingCount, IsNSNumber embeddingDimension, IsNSNumber paddingIndex, IsNSNumber maximumNorm, IsNSNumber pNorm) => embeddingCount -> embeddingDimension -> paddingIndex -> maximumNorm -> pNorm -> Bool -> IO (Id MLCEmbeddingDescriptor)
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequency embeddingCount embeddingDimension paddingIndex maximumNorm pNorm scalesGradientByFrequency =
  do
    cls' <- getRequiredClass "MLCEmbeddingDescriptor"
    withObjCPtr embeddingCount $ \raw_embeddingCount ->
      withObjCPtr embeddingDimension $ \raw_embeddingDimension ->
        withObjCPtr paddingIndex $ \raw_paddingIndex ->
          withObjCPtr maximumNorm $ \raw_maximumNorm ->
            withObjCPtr pNorm $ \raw_pNorm ->
              sendClassMsg cls' (mkSelector "descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:") (retPtr retVoid) [argPtr (castPtr raw_embeddingCount :: Ptr ()), argPtr (castPtr raw_embeddingDimension :: Ptr ()), argPtr (castPtr raw_paddingIndex :: Ptr ()), argPtr (castPtr raw_maximumNorm :: Ptr ()), argPtr (castPtr raw_pNorm :: Ptr ()), argCULong (if scalesGradientByFrequency then 1 else 0)] >>= retainedObject . castPtr

-- | embeddingCount
--
-- The size of the dictionary
--
-- ObjC selector: @- embeddingCount@
embeddingCount :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
embeddingCount mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "embeddingCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | embeddingDimension
--
-- The dimension of embedding vectors
--
-- ObjC selector: @- embeddingDimension@
embeddingDimension :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
embeddingDimension mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "embeddingDimension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | paddingIndex
--
-- If set, the embedding vector at paddingIndex is initialized with zero and will not be updated in gradient pass, Default=nil
--
-- ObjC selector: @- paddingIndex@
paddingIndex :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
paddingIndex mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "paddingIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maximumNorm
--
-- A float, if set, in the forward pass only, the selected embedding vectors will be re-normalized to have an Lp norm of less than maximumNorm in the dictionary, Default=nil
--
-- ObjC selector: @- maximumNorm@
maximumNorm :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
maximumNorm mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "maximumNorm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pNorm
--
-- A float, the p of the Lp norm, can be set to infinity norm by [NSNumber numberWithFloat:INFINITY]. Default=2.0
--
-- ObjC selector: @- pNorm@
pNorm :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO (Id NSNumber)
pNorm mlcEmbeddingDescriptor  =
    sendMsg mlcEmbeddingDescriptor (mkSelector "pNorm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scalesGradientByFrequency
--
-- If set, the gradients are scaled by the inverse of the frequency of the words in batch before the weight update. Default=NO
--
-- ObjC selector: @- scalesGradientByFrequency@
scalesGradientByFrequency :: IsMLCEmbeddingDescriptor mlcEmbeddingDescriptor => mlcEmbeddingDescriptor -> IO Bool
scalesGradientByFrequency mlcEmbeddingDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcEmbeddingDescriptor (mkSelector "scalesGradientByFrequency") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithEmbeddingCount:embeddingDimension:@
descriptorWithEmbeddingCount_embeddingDimensionSelector :: Selector
descriptorWithEmbeddingCount_embeddingDimensionSelector = mkSelector "descriptorWithEmbeddingCount:embeddingDimension:"

-- | @Selector@ for @descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:@
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector :: Selector
descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector = mkSelector "descriptorWithEmbeddingCount:embeddingDimension:paddingIndex:maximumNorm:pNorm:scalesGradientByFrequency:"

-- | @Selector@ for @embeddingCount@
embeddingCountSelector :: Selector
embeddingCountSelector = mkSelector "embeddingCount"

-- | @Selector@ for @embeddingDimension@
embeddingDimensionSelector :: Selector
embeddingDimensionSelector = mkSelector "embeddingDimension"

-- | @Selector@ for @paddingIndex@
paddingIndexSelector :: Selector
paddingIndexSelector = mkSelector "paddingIndex"

-- | @Selector@ for @maximumNorm@
maximumNormSelector :: Selector
maximumNormSelector = mkSelector "maximumNorm"

-- | @Selector@ for @pNorm@
pNormSelector :: Selector
pNormSelector = mkSelector "pNorm"

-- | @Selector@ for @scalesGradientByFrequency@
scalesGradientByFrequencySelector :: Selector
scalesGradientByFrequencySelector = mkSelector "scalesGradientByFrequency"


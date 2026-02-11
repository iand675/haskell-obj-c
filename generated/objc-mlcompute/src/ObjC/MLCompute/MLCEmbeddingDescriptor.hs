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
  , scalesGradientByFrequency
  , newSelector
  , initSelector
  , descriptorWithEmbeddingCount_embeddingDimensionSelector
  , descriptorWithEmbeddingCount_embeddingDimension_paddingIndex_maximumNorm_pNorm_scalesGradientByFrequencySelector
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

-- | @Selector@ for @scalesGradientByFrequency@
scalesGradientByFrequencySelector :: Selector
scalesGradientByFrequencySelector = mkSelector "scalesGradientByFrequency"


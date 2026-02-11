{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCMultiheadAttentionDescriptor
--
-- The MLCMultiheadAttentionDescriptor specifies a Multi-Head Attention descriptor
--
-- Generated bindings for @MLCMultiheadAttentionDescriptor@.
module ObjC.MLCompute.MLCMultiheadAttentionDescriptor
  ( MLCMultiheadAttentionDescriptor
  , IsMLCMultiheadAttentionDescriptor(..)
  , new
  , init_
  , descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttention
  , descriptorWithModelDimension_headCount
  , modelDimension
  , keyDimension
  , valueDimension
  , headCount
  , dropout
  , hasBiases
  , hasAttentionBiases
  , addsZeroAttention
  , newSelector
  , initSelector
  , descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector
  , descriptorWithModelDimension_headCountSelector
  , modelDimensionSelector
  , keyDimensionSelector
  , valueDimensionSelector
  , headCountSelector
  , dropoutSelector
  , hasBiasesSelector
  , hasAttentionBiasesSelector
  , addsZeroAttentionSelector


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
new :: IO (Id MLCMultiheadAttentionDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCMultiheadAttentionDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO (Id MLCMultiheadAttentionDescriptor)
init_ mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A multi-head attention layer descriptor
--
-- @modelDimension@ — total dimension of model space
--
-- @keyDimension@ — total dimension of key space. Default = modelDimension
--
-- @valueDimension@ — total dimension of value space. Default = modelDimension
--
-- @headCount@ — number of parallel attention heads
--
-- @dropout@ — optional, a dropout layer applied to the output projection weights. Default = 0.0f
--
-- @hasBiases@ — if true, bias will be added to query/key/value/output projections. Default = YES
--
-- @hasAttentionBiases@ — if true, an array of biases is added to key and value respectively. Default = NO
--
-- @addsZeroAttention@ — if true, a row of zeroes is added to projected key and value. Default = NO
--
-- Returns: A new MultiheadAttention layer descriptor
--
-- ObjC selector: @+ descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:@
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttention :: CULong -> CULong -> CULong -> CULong -> CFloat -> Bool -> Bool -> Bool -> IO (Id MLCMultiheadAttentionDescriptor)
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttention modelDimension keyDimension valueDimension headCount dropout hasBiases hasAttentionBiases addsZeroAttention =
  do
    cls' <- getRequiredClass "MLCMultiheadAttentionDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:") (retPtr retVoid) [argCULong (fromIntegral modelDimension), argCULong (fromIntegral keyDimension), argCULong (fromIntegral valueDimension), argCULong (fromIntegral headCount), argCFloat (fromIntegral dropout), argCULong (if hasBiases then 1 else 0), argCULong (if hasAttentionBiases then 1 else 0), argCULong (if addsZeroAttention then 1 else 0)] >>= retainedObject . castPtr

-- | A multi-head attention layer descriptor
--
-- @modelDimension@ — total dimension of model space
--
-- @headCount@ — number of parallel attention heads
--
-- Returns: A valid MultiheadAttention layer descriptor
--
-- ObjC selector: @+ descriptorWithModelDimension:headCount:@
descriptorWithModelDimension_headCount :: CULong -> CULong -> IO (Id MLCMultiheadAttentionDescriptor)
descriptorWithModelDimension_headCount modelDimension headCount =
  do
    cls' <- getRequiredClass "MLCMultiheadAttentionDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithModelDimension:headCount:") (retPtr retVoid) [argCULong (fromIntegral modelDimension), argCULong (fromIntegral headCount)] >>= retainedObject . castPtr

-- | model or embedding dimension
--
-- ObjC selector: @- modelDimension@
modelDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
modelDimension mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "modelDimension") retCULong []

-- | total dimension of key space, Default = modelDimension
--
-- ObjC selector: @- keyDimension@
keyDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
keyDimension mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "keyDimension") retCULong []

-- | total dimension of value space, Default = modelDimension
--
-- ObjC selector: @- valueDimension@
valueDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
valueDimension mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "valueDimension") retCULong []

-- | number of parallel attention heads
--
-- ObjC selector: @- headCount@
headCount :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
headCount mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "headCount") retCULong []

-- | a droupout layer applied to the output projection weights. Default = 0.0
--
-- ObjC selector: @- dropout@
dropout :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CFloat
dropout mlcMultiheadAttentionDescriptor  =
  sendMsg mlcMultiheadAttentionDescriptor (mkSelector "dropout") retCFloat []

-- | if true, bias is used for query/key/value/output projections. Default = true
--
-- ObjC selector: @- hasBiases@
hasBiases :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
hasBiases mlcMultiheadAttentionDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcMultiheadAttentionDescriptor (mkSelector "hasBiases") retCULong []

-- | if true, an array of biases is added to key and value respectively. Default = false
--
-- ObjC selector: @- hasAttentionBiases@
hasAttentionBiases :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
hasAttentionBiases mlcMultiheadAttentionDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcMultiheadAttentionDescriptor (mkSelector "hasAttentionBiases") retCULong []

-- | if true, a row of zeroes is added to projected key and value. Default = false
--
-- ObjC selector: @- addsZeroAttention@
addsZeroAttention :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
addsZeroAttention mlcMultiheadAttentionDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcMultiheadAttentionDescriptor (mkSelector "addsZeroAttention") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:@
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector :: Selector
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector = mkSelector "descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:"

-- | @Selector@ for @descriptorWithModelDimension:headCount:@
descriptorWithModelDimension_headCountSelector :: Selector
descriptorWithModelDimension_headCountSelector = mkSelector "descriptorWithModelDimension:headCount:"

-- | @Selector@ for @modelDimension@
modelDimensionSelector :: Selector
modelDimensionSelector = mkSelector "modelDimension"

-- | @Selector@ for @keyDimension@
keyDimensionSelector :: Selector
keyDimensionSelector = mkSelector "keyDimension"

-- | @Selector@ for @valueDimension@
valueDimensionSelector :: Selector
valueDimensionSelector = mkSelector "valueDimension"

-- | @Selector@ for @headCount@
headCountSelector :: Selector
headCountSelector = mkSelector "headCount"

-- | @Selector@ for @dropout@
dropoutSelector :: Selector
dropoutSelector = mkSelector "dropout"

-- | @Selector@ for @hasBiases@
hasBiasesSelector :: Selector
hasBiasesSelector = mkSelector "hasBiases"

-- | @Selector@ for @hasAttentionBiases@
hasAttentionBiasesSelector :: Selector
hasAttentionBiasesSelector = mkSelector "hasAttentionBiases"

-- | @Selector@ for @addsZeroAttention@
addsZeroAttentionSelector :: Selector
addsZeroAttentionSelector = mkSelector "addsZeroAttention"


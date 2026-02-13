{-# LANGUAGE DataKinds #-}
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
  , addsZeroAttentionSelector
  , descriptorWithModelDimension_headCountSelector
  , descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector
  , dropoutSelector
  , hasAttentionBiasesSelector
  , hasBiasesSelector
  , headCountSelector
  , initSelector
  , keyDimensionSelector
  , modelDimensionSelector
  , newSelector
  , valueDimensionSelector


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
new :: IO (Id MLCMultiheadAttentionDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCMultiheadAttentionDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO (Id MLCMultiheadAttentionDescriptor)
init_ mlcMultiheadAttentionDescriptor =
  sendOwnedMessage mlcMultiheadAttentionDescriptor initSelector

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
    sendClassMessage cls' descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector modelDimension keyDimension valueDimension headCount dropout hasBiases hasAttentionBiases addsZeroAttention

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
    sendClassMessage cls' descriptorWithModelDimension_headCountSelector modelDimension headCount

-- | model or embedding dimension
--
-- ObjC selector: @- modelDimension@
modelDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
modelDimension mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor modelDimensionSelector

-- | total dimension of key space, Default = modelDimension
--
-- ObjC selector: @- keyDimension@
keyDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
keyDimension mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor keyDimensionSelector

-- | total dimension of value space, Default = modelDimension
--
-- ObjC selector: @- valueDimension@
valueDimension :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
valueDimension mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor valueDimensionSelector

-- | number of parallel attention heads
--
-- ObjC selector: @- headCount@
headCount :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CULong
headCount mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor headCountSelector

-- | a droupout layer applied to the output projection weights. Default = 0.0
--
-- ObjC selector: @- dropout@
dropout :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO CFloat
dropout mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor dropoutSelector

-- | if true, bias is used for query/key/value/output projections. Default = true
--
-- ObjC selector: @- hasBiases@
hasBiases :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
hasBiases mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor hasBiasesSelector

-- | if true, an array of biases is added to key and value respectively. Default = false
--
-- ObjC selector: @- hasAttentionBiases@
hasAttentionBiases :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
hasAttentionBiases mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor hasAttentionBiasesSelector

-- | if true, a row of zeroes is added to projected key and value. Default = false
--
-- ObjC selector: @- addsZeroAttention@
addsZeroAttention :: IsMLCMultiheadAttentionDescriptor mlcMultiheadAttentionDescriptor => mlcMultiheadAttentionDescriptor -> IO Bool
addsZeroAttention mlcMultiheadAttentionDescriptor =
  sendMessage mlcMultiheadAttentionDescriptor addsZeroAttentionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCMultiheadAttentionDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCMultiheadAttentionDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:@
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector :: Selector '[CULong, CULong, CULong, CULong, CFloat, Bool, Bool, Bool] (Id MLCMultiheadAttentionDescriptor)
descriptorWithModelDimension_keyDimension_valueDimension_headCount_dropout_hasBiases_hasAttentionBiases_addsZeroAttentionSelector = mkSelector "descriptorWithModelDimension:keyDimension:valueDimension:headCount:dropout:hasBiases:hasAttentionBiases:addsZeroAttention:"

-- | @Selector@ for @descriptorWithModelDimension:headCount:@
descriptorWithModelDimension_headCountSelector :: Selector '[CULong, CULong] (Id MLCMultiheadAttentionDescriptor)
descriptorWithModelDimension_headCountSelector = mkSelector "descriptorWithModelDimension:headCount:"

-- | @Selector@ for @modelDimension@
modelDimensionSelector :: Selector '[] CULong
modelDimensionSelector = mkSelector "modelDimension"

-- | @Selector@ for @keyDimension@
keyDimensionSelector :: Selector '[] CULong
keyDimensionSelector = mkSelector "keyDimension"

-- | @Selector@ for @valueDimension@
valueDimensionSelector :: Selector '[] CULong
valueDimensionSelector = mkSelector "valueDimension"

-- | @Selector@ for @headCount@
headCountSelector :: Selector '[] CULong
headCountSelector = mkSelector "headCount"

-- | @Selector@ for @dropout@
dropoutSelector :: Selector '[] CFloat
dropoutSelector = mkSelector "dropout"

-- | @Selector@ for @hasBiases@
hasBiasesSelector :: Selector '[] Bool
hasBiasesSelector = mkSelector "hasBiases"

-- | @Selector@ for @hasAttentionBiases@
hasAttentionBiasesSelector :: Selector '[] Bool
hasAttentionBiasesSelector = mkSelector "hasAttentionBiases"

-- | @Selector@ for @addsZeroAttention@
addsZeroAttentionSelector :: Selector '[] Bool
addsZeroAttentionSelector = mkSelector "addsZeroAttention"


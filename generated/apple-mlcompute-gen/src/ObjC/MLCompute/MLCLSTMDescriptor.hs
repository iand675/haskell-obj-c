{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLSTMDescriptor
--
-- The MLCLSTMDescriptor specifies a LSTM descriptor
--
-- Generated bindings for @MLCLSTMDescriptor@.
module ObjC.MLCompute.MLCLSTMDescriptor
  ( MLCLSTMDescriptor
  , IsMLCLSTMDescriptor(..)
  , new
  , init_
  , descriptorWithInputSize_hiddenSize_layerCount
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropout
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropout
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultMode
  , inputSize
  , hiddenSize
  , layerCount
  , usesBiases
  , batchFirst
  , isBidirectional
  , returnsSequences
  , dropout
  , resultMode
  , batchFirstSelector
  , descriptorWithInputSize_hiddenSize_layerCountSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector
  , dropoutSelector
  , hiddenSizeSelector
  , initSelector
  , inputSizeSelector
  , isBidirectionalSelector
  , layerCountSelector
  , newSelector
  , resultModeSelector
  , returnsSequencesSelector
  , usesBiasesSelector

  -- * Enum types
  , MLCLSTMResultMode(MLCLSTMResultMode)
  , pattern MLCLSTMResultModeOutput
  , pattern MLCLSTMResultModeOutputAndStates

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCLSTMDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO (Id MLCLSTMDescriptor)
init_ mlclstmDescriptor =
  sendOwnedMessage mlclstmDescriptor initSelector

-- | Creates a LSTM descriptor with batchFirst = YES
--
-- @inputSize@ — The number of expected features in the input
--
-- @hiddenSize@ — The number of features in the hidden state
--
-- @layerCount@ — Number of recurrent layers
--
-- Returns: A valid MLCLSTMDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithInputSize:hiddenSize:layerCount:@
descriptorWithInputSize_hiddenSize_layerCount :: CULong -> CULong -> CULong -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount inputSize hiddenSize layerCount =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMessage cls' descriptorWithInputSize_hiddenSize_layerCountSelector inputSize hiddenSize layerCount

-- | Creates a LSTM descriptor descriptor with batchFirst = YES
--
-- @inputSize@ — The number of expected features in the input
--
-- @hiddenSize@ — The number of features in the hidden state
--
-- @layerCount@ — Number of recurrent layers
--
-- @usesBiases@ — If NO, the layer does not use bias weights.  Default: YES
--
-- @isBidirectional@ — If YES, becomes a bi-directional LSTM.  Default: NO
--
-- @dropout@ — If non-zero, introduces a dropout layer on the outputs of each LSTM layer except the last layer with dropout probability equal to dropout.
--
-- Returns: A valid MLCLSTMDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropout :: CULong -> CULong -> CULong -> Bool -> Bool -> CFloat -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropout inputSize hiddenSize layerCount usesBiases isBidirectional dropout =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMessage cls' descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector inputSize hiddenSize layerCount usesBiases isBidirectional dropout

-- | @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropout :: CULong -> CULong -> CULong -> Bool -> Bool -> Bool -> CFloat -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropout inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional dropout =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMessage cls' descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional dropout

-- | @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout :: CULong -> CULong -> CULong -> Bool -> Bool -> Bool -> Bool -> CFloat -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional returnsSequences dropout =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMessage cls' descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional returnsSequences dropout

-- | Creates a LSTM descriptor.
--
-- @inputSize@ — The number of expected features in the input
--
-- @hiddenSize@ — The number of features in the hidden state
--
-- @layerCount@ — Number of recurrent layers
--
-- @usesBiases@ — If NO, the layer does not use bias weights.  Default: YES
--
-- @batchFirst@ — LSTM only supports batchFirst=YES. This means the input and output will have shape [batch size, time steps, feature]. Default is YES.
--
-- @isBidirectional@ — If YES, becomes a bi-directional LSTM.  Default: NO
--
-- @returnsSequences@ — if YES return output for all sequences else return output only for the last sequences. Default: YES
--
-- @dropout@ — If non-zero, introduces a dropout layer on the outputs of each LSTM layer except the last layer with dropout probability equal to dropout.
--
-- @resultMode@ — expected result tensors. MLCLSTMResultModeOutput returns output data. MLCLSTMResultModeOutputAndStates returns             output data, last hidden state h_n, and last cell state c_n. Default: MLCLSTMResultModeOutput.
--
-- Returns: A valid MLCLSTMDescriptor object or nil, if failure.
--
-- ObjC selector: @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultMode :: CULong -> CULong -> CULong -> Bool -> Bool -> Bool -> Bool -> CFloat -> MLCLSTMResultMode -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultMode inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional returnsSequences dropout resultMode =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMessage cls' descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional returnsSequences dropout resultMode

-- | inputSize
--
-- The number of expected feature channels in the input
--
-- ObjC selector: @- inputSize@
inputSize :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
inputSize mlclstmDescriptor =
  sendMessage mlclstmDescriptor inputSizeSelector

-- | hiddenSize
--
-- The number of feature channels in the hidden state
--
-- ObjC selector: @- hiddenSize@
hiddenSize :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
hiddenSize mlclstmDescriptor =
  sendMessage mlclstmDescriptor hiddenSizeSelector

-- | layerCount
--
-- The number of recurrent layers.  Default is 1.
--
-- ObjC selector: @- layerCount@
layerCount :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
layerCount mlclstmDescriptor =
  sendMessage mlclstmDescriptor layerCountSelector

-- | usesBiases
--
-- If NO, the layer does not use bias terms.  Default is YES.
--
-- ObjC selector: @- usesBiases@
usesBiases :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
usesBiases mlclstmDescriptor =
  sendMessage mlclstmDescriptor usesBiasesSelector

-- | batchFirst
--
-- LSTM only supports batchFirst=YES. This means the input and output will have shape [batch size, time steps, feature]. Default is YES.
--
-- ObjC selector: @- batchFirst@
batchFirst :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
batchFirst mlclstmDescriptor =
  sendMessage mlclstmDescriptor batchFirstSelector

-- | isBidirectional
--
-- If YES, becomes a bidirectional LSTM.  Default is NO.
--
-- ObjC selector: @- isBidirectional@
isBidirectional :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
isBidirectional mlclstmDescriptor =
  sendMessage mlclstmDescriptor isBidirectionalSelector

-- | returnsSequences
--
-- if YES return output for all sequences else return output only for the last sequences. Default: YES
--
-- ObjC selector: @- returnsSequences@
returnsSequences :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
returnsSequences mlclstmDescriptor =
  sendMessage mlclstmDescriptor returnsSequencesSelector

-- | dropout
--
-- If non-zero, intrdouces a dropout layer on the outputs of each LSTM layer                except the last layer, with dropout probablity equal to dropout.  Default is 0.0.
--
-- ObjC selector: @- dropout@
dropout :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CFloat
dropout mlclstmDescriptor =
  sendMessage mlclstmDescriptor dropoutSelector

-- | resultMode
--
-- MLCLSTMResultModeOutput returns output data. MLCLSTMResultModeOutputAndStates returns                output data, last hidden state h_n, and last cell state c_n. Default MLCLSTMResultModeOutput.
--
-- ObjC selector: @- resultMode@
resultMode :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO MLCLSTMResultMode
resultMode mlclstmDescriptor =
  sendMessage mlclstmDescriptor resultModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCLSTMDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCLSTMDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:@
descriptorWithInputSize_hiddenSize_layerCountSelector :: Selector '[CULong, CULong, CULong] (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCountSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector :: Selector '[CULong, CULong, CULong, Bool, Bool, CFloat] (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector :: Selector '[CULong, CULong, CULong, Bool, Bool, Bool, CFloat] (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector :: Selector '[CULong, CULong, CULong, Bool, Bool, Bool, Bool, CFloat] (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector :: Selector '[CULong, CULong, CULong, Bool, Bool, Bool, Bool, CFloat, MLCLSTMResultMode] (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:"

-- | @Selector@ for @inputSize@
inputSizeSelector :: Selector '[] CULong
inputSizeSelector = mkSelector "inputSize"

-- | @Selector@ for @hiddenSize@
hiddenSizeSelector :: Selector '[] CULong
hiddenSizeSelector = mkSelector "hiddenSize"

-- | @Selector@ for @layerCount@
layerCountSelector :: Selector '[] CULong
layerCountSelector = mkSelector "layerCount"

-- | @Selector@ for @usesBiases@
usesBiasesSelector :: Selector '[] Bool
usesBiasesSelector = mkSelector "usesBiases"

-- | @Selector@ for @batchFirst@
batchFirstSelector :: Selector '[] Bool
batchFirstSelector = mkSelector "batchFirst"

-- | @Selector@ for @isBidirectional@
isBidirectionalSelector :: Selector '[] Bool
isBidirectionalSelector = mkSelector "isBidirectional"

-- | @Selector@ for @returnsSequences@
returnsSequencesSelector :: Selector '[] Bool
returnsSequencesSelector = mkSelector "returnsSequences"

-- | @Selector@ for @dropout@
dropoutSelector :: Selector '[] CFloat
dropoutSelector = mkSelector "dropout"

-- | @Selector@ for @resultMode@
resultModeSelector :: Selector '[] MLCLSTMResultMode
resultModeSelector = mkSelector "resultMode"


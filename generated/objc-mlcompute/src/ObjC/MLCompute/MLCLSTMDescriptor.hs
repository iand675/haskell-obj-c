{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , descriptorWithInputSize_hiddenSize_layerCountSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector
  , descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector
  , inputSizeSelector
  , hiddenSizeSelector
  , layerCountSelector
  , usesBiasesSelector
  , batchFirstSelector
  , isBidirectionalSelector
  , returnsSequencesSelector
  , dropoutSelector
  , resultModeSelector

  -- * Enum types
  , MLCLSTMResultMode(MLCLSTMResultMode)
  , pattern MLCLSTMResultModeOutput
  , pattern MLCLSTMResultModeOutputAndStates

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
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCLSTMDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO (Id MLCLSTMDescriptor)
init_ mlclstmDescriptor  =
  sendMsg mlclstmDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithInputSize:hiddenSize:layerCount:") (retPtr retVoid) [argCULong (fromIntegral inputSize), argCULong (fromIntegral hiddenSize), argCULong (fromIntegral layerCount)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:") (retPtr retVoid) [argCULong (fromIntegral inputSize), argCULong (fromIntegral hiddenSize), argCULong (fromIntegral layerCount), argCULong (if usesBiases then 1 else 0), argCULong (if isBidirectional then 1 else 0), argCFloat (fromIntegral dropout)] >>= retainedObject . castPtr

-- | @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropout :: CULong -> CULong -> CULong -> Bool -> Bool -> Bool -> CFloat -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropout inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional dropout =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:") (retPtr retVoid) [argCULong (fromIntegral inputSize), argCULong (fromIntegral hiddenSize), argCULong (fromIntegral layerCount), argCULong (if usesBiases then 1 else 0), argCULong (if batchFirst then 1 else 0), argCULong (if isBidirectional then 1 else 0), argCFloat (fromIntegral dropout)] >>= retainedObject . castPtr

-- | @+ descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout :: CULong -> CULong -> CULong -> Bool -> Bool -> Bool -> Bool -> CFloat -> IO (Id MLCLSTMDescriptor)
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout inputSize hiddenSize layerCount usesBiases batchFirst isBidirectional returnsSequences dropout =
  do
    cls' <- getRequiredClass "MLCLSTMDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:") (retPtr retVoid) [argCULong (fromIntegral inputSize), argCULong (fromIntegral hiddenSize), argCULong (fromIntegral layerCount), argCULong (if usesBiases then 1 else 0), argCULong (if batchFirst then 1 else 0), argCULong (if isBidirectional then 1 else 0), argCULong (if returnsSequences then 1 else 0), argCFloat (fromIntegral dropout)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:") (retPtr retVoid) [argCULong (fromIntegral inputSize), argCULong (fromIntegral hiddenSize), argCULong (fromIntegral layerCount), argCULong (if usesBiases then 1 else 0), argCULong (if batchFirst then 1 else 0), argCULong (if isBidirectional then 1 else 0), argCULong (if returnsSequences then 1 else 0), argCFloat (fromIntegral dropout), argCULong (coerce resultMode)] >>= retainedObject . castPtr

-- | inputSize
--
-- The number of expected feature channels in the input
--
-- ObjC selector: @- inputSize@
inputSize :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
inputSize mlclstmDescriptor  =
  sendMsg mlclstmDescriptor (mkSelector "inputSize") retCULong []

-- | hiddenSize
--
-- The number of feature channels in the hidden state
--
-- ObjC selector: @- hiddenSize@
hiddenSize :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
hiddenSize mlclstmDescriptor  =
  sendMsg mlclstmDescriptor (mkSelector "hiddenSize") retCULong []

-- | layerCount
--
-- The number of recurrent layers.  Default is 1.
--
-- ObjC selector: @- layerCount@
layerCount :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CULong
layerCount mlclstmDescriptor  =
  sendMsg mlclstmDescriptor (mkSelector "layerCount") retCULong []

-- | usesBiases
--
-- If NO, the layer does not use bias terms.  Default is YES.
--
-- ObjC selector: @- usesBiases@
usesBiases :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
usesBiases mlclstmDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlclstmDescriptor (mkSelector "usesBiases") retCULong []

-- | batchFirst
--
-- LSTM only supports batchFirst=YES. This means the input and output will have shape [batch size, time steps, feature]. Default is YES.
--
-- ObjC selector: @- batchFirst@
batchFirst :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
batchFirst mlclstmDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlclstmDescriptor (mkSelector "batchFirst") retCULong []

-- | isBidirectional
--
-- If YES, becomes a bidirectional LSTM.  Default is NO.
--
-- ObjC selector: @- isBidirectional@
isBidirectional :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
isBidirectional mlclstmDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlclstmDescriptor (mkSelector "isBidirectional") retCULong []

-- | returnsSequences
--
-- if YES return output for all sequences else return output only for the last sequences. Default: YES
--
-- ObjC selector: @- returnsSequences@
returnsSequences :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO Bool
returnsSequences mlclstmDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlclstmDescriptor (mkSelector "returnsSequences") retCULong []

-- | dropout
--
-- If non-zero, intrdouces a dropout layer on the outputs of each LSTM layer                except the last layer, with dropout probablity equal to dropout.  Default is 0.0.
--
-- ObjC selector: @- dropout@
dropout :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO CFloat
dropout mlclstmDescriptor  =
  sendMsg mlclstmDescriptor (mkSelector "dropout") retCFloat []

-- | resultMode
--
-- MLCLSTMResultModeOutput returns output data. MLCLSTMResultModeOutputAndStates returns                output data, last hidden state h_n, and last cell state c_n. Default MLCLSTMResultModeOutput.
--
-- ObjC selector: @- resultMode@
resultMode :: IsMLCLSTMDescriptor mlclstmDescriptor => mlclstmDescriptor -> IO MLCLSTMResultMode
resultMode mlclstmDescriptor  =
  fmap (coerce :: CULong -> MLCLSTMResultMode) $ sendMsg mlclstmDescriptor (mkSelector "resultMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:@
descriptorWithInputSize_hiddenSize_layerCountSelector :: Selector
descriptorWithInputSize_hiddenSize_layerCountSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector :: Selector
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_isBidirectional_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:isBidirectional:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector :: Selector
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector :: Selector
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropoutSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:"

-- | @Selector@ for @descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:@
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector :: Selector
descriptorWithInputSize_hiddenSize_layerCount_usesBiases_batchFirst_isBidirectional_returnsSequences_dropout_resultModeSelector = mkSelector "descriptorWithInputSize:hiddenSize:layerCount:usesBiases:batchFirst:isBidirectional:returnsSequences:dropout:resultMode:"

-- | @Selector@ for @inputSize@
inputSizeSelector :: Selector
inputSizeSelector = mkSelector "inputSize"

-- | @Selector@ for @hiddenSize@
hiddenSizeSelector :: Selector
hiddenSizeSelector = mkSelector "hiddenSize"

-- | @Selector@ for @layerCount@
layerCountSelector :: Selector
layerCountSelector = mkSelector "layerCount"

-- | @Selector@ for @usesBiases@
usesBiasesSelector :: Selector
usesBiasesSelector = mkSelector "usesBiases"

-- | @Selector@ for @batchFirst@
batchFirstSelector :: Selector
batchFirstSelector = mkSelector "batchFirst"

-- | @Selector@ for @isBidirectional@
isBidirectionalSelector :: Selector
isBidirectionalSelector = mkSelector "isBidirectional"

-- | @Selector@ for @returnsSequences@
returnsSequencesSelector :: Selector
returnsSequencesSelector = mkSelector "returnsSequences"

-- | @Selector@ for @dropout@
dropoutSelector :: Selector
dropoutSelector = mkSelector "dropout"

-- | @Selector@ for @resultMode@
resultModeSelector :: Selector
resultModeSelector = mkSelector "resultMode"


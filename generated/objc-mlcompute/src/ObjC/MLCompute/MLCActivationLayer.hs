{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLCActivationLayer@.
module ObjC.MLCompute.MLCActivationLayer
  ( MLCActivationLayer
  , IsMLCActivationLayer(..)
  , layerWithDescriptor
  , leakyReLULayerWithNegativeSlope
  , linearLayerWithScale_bias
  , softPlusLayerWithBeta
  , eluLayerWithA
  , relunLayerWithA_b
  , celuLayerWithA
  , hardShrinkLayerWithA
  , softShrinkLayerWithA
  , thresholdLayerWithThreshold_replacement
  , clampLayerWithMinValue_maxValue
  , descriptor
  , reluLayer
  , relu6Layer
  , leakyReLULayer
  , sigmoidLayer
  , hardSigmoidLayer
  , tanhLayer
  , absoluteLayer
  , softPlusLayer
  , softSignLayer
  , eluLayer
  , logSigmoidLayer
  , seluLayer
  , celuLayer
  , hardShrinkLayer
  , softShrinkLayer
  , tanhShrinkLayer
  , geluLayer
  , hardSwishLayer
  , layerWithDescriptorSelector
  , leakyReLULayerWithNegativeSlopeSelector
  , linearLayerWithScale_biasSelector
  , softPlusLayerWithBetaSelector
  , eluLayerWithASelector
  , relunLayerWithA_bSelector
  , celuLayerWithASelector
  , hardShrinkLayerWithASelector
  , softShrinkLayerWithASelector
  , thresholdLayerWithThreshold_replacementSelector
  , clampLayerWithMinValue_maxValueSelector
  , descriptorSelector
  , reluLayerSelector
  , relu6LayerSelector
  , leakyReLULayerSelector
  , sigmoidLayerSelector
  , hardSigmoidLayerSelector
  , tanhLayerSelector
  , absoluteLayerSelector
  , softPlusLayerSelector
  , softSignLayerSelector
  , eluLayerSelector
  , logSigmoidLayerSelector
  , seluLayerSelector
  , celuLayerSelector
  , hardShrinkLayerSelector
  , softShrinkLayerSelector
  , tanhShrinkLayerSelector
  , geluLayerSelector
  , hardSwishLayerSelector


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

-- | Create an activation layer
--
-- @descriptor@ — The activation descriptor
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ layerWithDescriptor:@
layerWithDescriptor :: IsMLCActivationDescriptor descriptor => descriptor -> IO (Id MLCActivationLayer)
layerWithDescriptor descriptor =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "layerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Create a leaky ReLU activation layer
--
-- @negativeSlope@ — Controls the angle of the negative slope
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ leakyReLULayerWithNegativeSlope:@
leakyReLULayerWithNegativeSlope :: CFloat -> IO (Id MLCActivationLayer)
leakyReLULayerWithNegativeSlope negativeSlope =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "leakyReLULayerWithNegativeSlope:") (retPtr retVoid) [argCFloat (fromIntegral negativeSlope)] >>= retainedObject . castPtr

-- | Create a linear activation layer
--
-- @scale@ — The scale factor
--
-- @bias@ — The bias value
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ linearLayerWithScale:bias:@
linearLayerWithScale_bias :: CFloat -> CFloat -> IO (Id MLCActivationLayer)
linearLayerWithScale_bias scale bias =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "linearLayerWithScale:bias:") (retPtr retVoid) [argCFloat (fromIntegral scale), argCFloat (fromIntegral bias)] >>= retainedObject . castPtr

-- | Create a soft plus activation layer
--
-- @beta@ — The beta value for the softplus formation
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softPlusLayerWithBeta:@
softPlusLayerWithBeta :: CFloat -> IO (Id MLCActivationLayer)
softPlusLayerWithBeta beta =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "softPlusLayerWithBeta:") (retPtr retVoid) [argCFloat (fromIntegral beta)] >>= retainedObject . castPtr

-- | Create an ELU activation layer
--
-- @a@ — The @a@ value for the ELU formation
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ eluLayerWithA:@
eluLayerWithA :: CFloat -> IO (Id MLCActivationLayer)
eluLayerWithA a =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "eluLayerWithA:") (retPtr retVoid) [argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create a ReLUN activation layer
--
-- This can be used to implement layers such as ReLU6 for example.
--
-- @a@ — The @a@ value
--
-- @b@ — The @b@ value
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ relunLayerWithA:b:@
relunLayerWithA_b :: CFloat -> CFloat -> IO (Id MLCActivationLayer)
relunLayerWithA_b a b =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "relunLayerWithA:b:") (retPtr retVoid) [argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= retainedObject . castPtr

-- | Create a CELU activation layer
--
-- @a@ — The @a@ value for the CELU formation
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ celuLayerWithA:@
celuLayerWithA :: CFloat -> IO (Id MLCActivationLayer)
celuLayerWithA a =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "celuLayerWithA:") (retPtr retVoid) [argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create a hard shrink activation layer
--
-- @a@ — The @a@ value for the hard shrink formation
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardShrinkLayerWithA:@
hardShrinkLayerWithA :: CFloat -> IO (Id MLCActivationLayer)
hardShrinkLayerWithA a =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "hardShrinkLayerWithA:") (retPtr retVoid) [argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create a soft shrink activation layer
--
-- @a@ — The @a@ value for the soft shrink formation
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softShrinkLayerWithA:@
softShrinkLayerWithA :: CFloat -> IO (Id MLCActivationLayer)
softShrinkLayerWithA a =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "softShrinkLayerWithA:") (retPtr retVoid) [argCFloat (fromIntegral a)] >>= retainedObject . castPtr

-- | Create a threshold activation layer
--
-- @threshold@ — The value to threshold at
--
-- @replacement@ — The value to replace with
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ thresholdLayerWithThreshold:replacement:@
thresholdLayerWithThreshold_replacement :: CFloat -> CFloat -> IO (Id MLCActivationLayer)
thresholdLayerWithThreshold_replacement threshold replacement =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "thresholdLayerWithThreshold:replacement:") (retPtr retVoid) [argCFloat (fromIntegral threshold), argCFloat (fromIntegral replacement)] >>= retainedObject . castPtr

-- | Create a clamp activation layer
--
-- @minValue@ — The minimum range used by clamp
--
-- @maxValue@ — The maximum range used by clamp
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ clampLayerWithMinValue:maxValue:@
clampLayerWithMinValue_maxValue :: CFloat -> CFloat -> IO (Id MLCActivationLayer)
clampLayerWithMinValue_maxValue minValue maxValue =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "clampLayerWithMinValue:maxValue:") (retPtr retVoid) [argCFloat (fromIntegral minValue), argCFloat (fromIntegral maxValue)] >>= retainedObject . castPtr

-- | descriptor
--
-- The activation descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCActivationLayer mlcActivationLayer => mlcActivationLayer -> IO (Id MLCActivationDescriptor)
descriptor mlcActivationLayer  =
  sendMsg mlcActivationLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a ReLU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ reluLayer@
reluLayer :: IO (Id MLCActivationLayer)
reluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "reluLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a ReLU6 activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ relu6Layer@
relu6Layer :: IO (Id MLCActivationLayer)
relu6Layer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "relu6Layer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a leaky ReLU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ leakyReLULayer@
leakyReLULayer :: IO (Id MLCActivationLayer)
leakyReLULayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "leakyReLULayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ sigmoidLayer@
sigmoidLayer :: IO (Id MLCActivationLayer)
sigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "sigmoidLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a hard sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardSigmoidLayer@
hardSigmoidLayer :: IO (Id MLCActivationLayer)
hardSigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "hardSigmoidLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a tanh activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ tanhLayer@
tanhLayer :: IO (Id MLCActivationLayer)
tanhLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "tanhLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an absolute activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ absoluteLayer@
absoluteLayer :: IO (Id MLCActivationLayer)
absoluteLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "absoluteLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a soft plus activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softPlusLayer@
softPlusLayer :: IO (Id MLCActivationLayer)
softPlusLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "softPlusLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a soft sign activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softSignLayer@
softSignLayer :: IO (Id MLCActivationLayer)
softSignLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "softSignLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an ELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ eluLayer@
eluLayer :: IO (Id MLCActivationLayer)
eluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "eluLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a log sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ logSigmoidLayer@
logSigmoidLayer :: IO (Id MLCActivationLayer)
logSigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "logSigmoidLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a SELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ seluLayer@
seluLayer :: IO (Id MLCActivationLayer)
seluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "seluLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a CELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ celuLayer@
celuLayer :: IO (Id MLCActivationLayer)
celuLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "celuLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a hard shrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardShrinkLayer@
hardShrinkLayer :: IO (Id MLCActivationLayer)
hardShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "hardShrinkLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a soft shrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softShrinkLayer@
softShrinkLayer :: IO (Id MLCActivationLayer)
softShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "softShrinkLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a TanhShrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ tanhShrinkLayer@
tanhShrinkLayer :: IO (Id MLCActivationLayer)
tanhShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "tanhShrinkLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a GELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ geluLayer@
geluLayer :: IO (Id MLCActivationLayer)
geluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "geluLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a hardswish activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardSwishLayer@
hardSwishLayer :: IO (Id MLCActivationLayer)
hardSwishLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMsg cls' (mkSelector "hardSwishLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @leakyReLULayerWithNegativeSlope:@
leakyReLULayerWithNegativeSlopeSelector :: Selector
leakyReLULayerWithNegativeSlopeSelector = mkSelector "leakyReLULayerWithNegativeSlope:"

-- | @Selector@ for @linearLayerWithScale:bias:@
linearLayerWithScale_biasSelector :: Selector
linearLayerWithScale_biasSelector = mkSelector "linearLayerWithScale:bias:"

-- | @Selector@ for @softPlusLayerWithBeta:@
softPlusLayerWithBetaSelector :: Selector
softPlusLayerWithBetaSelector = mkSelector "softPlusLayerWithBeta:"

-- | @Selector@ for @eluLayerWithA:@
eluLayerWithASelector :: Selector
eluLayerWithASelector = mkSelector "eluLayerWithA:"

-- | @Selector@ for @relunLayerWithA:b:@
relunLayerWithA_bSelector :: Selector
relunLayerWithA_bSelector = mkSelector "relunLayerWithA:b:"

-- | @Selector@ for @celuLayerWithA:@
celuLayerWithASelector :: Selector
celuLayerWithASelector = mkSelector "celuLayerWithA:"

-- | @Selector@ for @hardShrinkLayerWithA:@
hardShrinkLayerWithASelector :: Selector
hardShrinkLayerWithASelector = mkSelector "hardShrinkLayerWithA:"

-- | @Selector@ for @softShrinkLayerWithA:@
softShrinkLayerWithASelector :: Selector
softShrinkLayerWithASelector = mkSelector "softShrinkLayerWithA:"

-- | @Selector@ for @thresholdLayerWithThreshold:replacement:@
thresholdLayerWithThreshold_replacementSelector :: Selector
thresholdLayerWithThreshold_replacementSelector = mkSelector "thresholdLayerWithThreshold:replacement:"

-- | @Selector@ for @clampLayerWithMinValue:maxValue:@
clampLayerWithMinValue_maxValueSelector :: Selector
clampLayerWithMinValue_maxValueSelector = mkSelector "clampLayerWithMinValue:maxValue:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @reluLayer@
reluLayerSelector :: Selector
reluLayerSelector = mkSelector "reluLayer"

-- | @Selector@ for @relu6Layer@
relu6LayerSelector :: Selector
relu6LayerSelector = mkSelector "relu6Layer"

-- | @Selector@ for @leakyReLULayer@
leakyReLULayerSelector :: Selector
leakyReLULayerSelector = mkSelector "leakyReLULayer"

-- | @Selector@ for @sigmoidLayer@
sigmoidLayerSelector :: Selector
sigmoidLayerSelector = mkSelector "sigmoidLayer"

-- | @Selector@ for @hardSigmoidLayer@
hardSigmoidLayerSelector :: Selector
hardSigmoidLayerSelector = mkSelector "hardSigmoidLayer"

-- | @Selector@ for @tanhLayer@
tanhLayerSelector :: Selector
tanhLayerSelector = mkSelector "tanhLayer"

-- | @Selector@ for @absoluteLayer@
absoluteLayerSelector :: Selector
absoluteLayerSelector = mkSelector "absoluteLayer"

-- | @Selector@ for @softPlusLayer@
softPlusLayerSelector :: Selector
softPlusLayerSelector = mkSelector "softPlusLayer"

-- | @Selector@ for @softSignLayer@
softSignLayerSelector :: Selector
softSignLayerSelector = mkSelector "softSignLayer"

-- | @Selector@ for @eluLayer@
eluLayerSelector :: Selector
eluLayerSelector = mkSelector "eluLayer"

-- | @Selector@ for @logSigmoidLayer@
logSigmoidLayerSelector :: Selector
logSigmoidLayerSelector = mkSelector "logSigmoidLayer"

-- | @Selector@ for @seluLayer@
seluLayerSelector :: Selector
seluLayerSelector = mkSelector "seluLayer"

-- | @Selector@ for @celuLayer@
celuLayerSelector :: Selector
celuLayerSelector = mkSelector "celuLayer"

-- | @Selector@ for @hardShrinkLayer@
hardShrinkLayerSelector :: Selector
hardShrinkLayerSelector = mkSelector "hardShrinkLayer"

-- | @Selector@ for @softShrinkLayer@
softShrinkLayerSelector :: Selector
softShrinkLayerSelector = mkSelector "softShrinkLayer"

-- | @Selector@ for @tanhShrinkLayer@
tanhShrinkLayerSelector :: Selector
tanhShrinkLayerSelector = mkSelector "tanhShrinkLayer"

-- | @Selector@ for @geluLayer@
geluLayerSelector :: Selector
geluLayerSelector = mkSelector "geluLayer"

-- | @Selector@ for @hardSwishLayer@
hardSwishLayerSelector :: Selector
hardSwishLayerSelector = mkSelector "hardSwishLayer"


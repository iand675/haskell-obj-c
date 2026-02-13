{-# LANGUAGE DataKinds #-}
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
  , absoluteLayerSelector
  , celuLayerSelector
  , celuLayerWithASelector
  , clampLayerWithMinValue_maxValueSelector
  , descriptorSelector
  , eluLayerSelector
  , eluLayerWithASelector
  , geluLayerSelector
  , hardShrinkLayerSelector
  , hardShrinkLayerWithASelector
  , hardSigmoidLayerSelector
  , hardSwishLayerSelector
  , layerWithDescriptorSelector
  , leakyReLULayerSelector
  , leakyReLULayerWithNegativeSlopeSelector
  , linearLayerWithScale_biasSelector
  , logSigmoidLayerSelector
  , relu6LayerSelector
  , reluLayerSelector
  , relunLayerWithA_bSelector
  , seluLayerSelector
  , sigmoidLayerSelector
  , softPlusLayerSelector
  , softPlusLayerWithBetaSelector
  , softShrinkLayerSelector
  , softShrinkLayerWithASelector
  , softSignLayerSelector
  , tanhLayerSelector
  , tanhShrinkLayerSelector
  , thresholdLayerWithThreshold_replacementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' layerWithDescriptorSelector (toMLCActivationDescriptor descriptor)

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
    sendClassMessage cls' leakyReLULayerWithNegativeSlopeSelector negativeSlope

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
    sendClassMessage cls' linearLayerWithScale_biasSelector scale bias

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
    sendClassMessage cls' softPlusLayerWithBetaSelector beta

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
    sendClassMessage cls' eluLayerWithASelector a

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
    sendClassMessage cls' relunLayerWithA_bSelector a b

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
    sendClassMessage cls' celuLayerWithASelector a

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
    sendClassMessage cls' hardShrinkLayerWithASelector a

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
    sendClassMessage cls' softShrinkLayerWithASelector a

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
    sendClassMessage cls' thresholdLayerWithThreshold_replacementSelector threshold replacement

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
    sendClassMessage cls' clampLayerWithMinValue_maxValueSelector minValue maxValue

-- | descriptor
--
-- The activation descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCActivationLayer mlcActivationLayer => mlcActivationLayer -> IO (Id MLCActivationDescriptor)
descriptor mlcActivationLayer =
  sendMessage mlcActivationLayer descriptorSelector

-- | Create a ReLU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ reluLayer@
reluLayer :: IO (Id MLCActivationLayer)
reluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' reluLayerSelector

-- | Create a ReLU6 activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ relu6Layer@
relu6Layer :: IO (Id MLCActivationLayer)
relu6Layer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' relu6LayerSelector

-- | Create a leaky ReLU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ leakyReLULayer@
leakyReLULayer :: IO (Id MLCActivationLayer)
leakyReLULayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' leakyReLULayerSelector

-- | Create a sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ sigmoidLayer@
sigmoidLayer :: IO (Id MLCActivationLayer)
sigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' sigmoidLayerSelector

-- | Create a hard sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardSigmoidLayer@
hardSigmoidLayer :: IO (Id MLCActivationLayer)
hardSigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' hardSigmoidLayerSelector

-- | Create a tanh activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ tanhLayer@
tanhLayer :: IO (Id MLCActivationLayer)
tanhLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' tanhLayerSelector

-- | Create an absolute activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ absoluteLayer@
absoluteLayer :: IO (Id MLCActivationLayer)
absoluteLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' absoluteLayerSelector

-- | Create a soft plus activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softPlusLayer@
softPlusLayer :: IO (Id MLCActivationLayer)
softPlusLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' softPlusLayerSelector

-- | Create a soft sign activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softSignLayer@
softSignLayer :: IO (Id MLCActivationLayer)
softSignLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' softSignLayerSelector

-- | Create an ELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ eluLayer@
eluLayer :: IO (Id MLCActivationLayer)
eluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' eluLayerSelector

-- | Create a log sigmoid activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ logSigmoidLayer@
logSigmoidLayer :: IO (Id MLCActivationLayer)
logSigmoidLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' logSigmoidLayerSelector

-- | Create a SELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ seluLayer@
seluLayer :: IO (Id MLCActivationLayer)
seluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' seluLayerSelector

-- | Create a CELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ celuLayer@
celuLayer :: IO (Id MLCActivationLayer)
celuLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' celuLayerSelector

-- | Create a hard shrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardShrinkLayer@
hardShrinkLayer :: IO (Id MLCActivationLayer)
hardShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' hardShrinkLayerSelector

-- | Create a soft shrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ softShrinkLayer@
softShrinkLayer :: IO (Id MLCActivationLayer)
softShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' softShrinkLayerSelector

-- | Create a TanhShrink activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ tanhShrinkLayer@
tanhShrinkLayer :: IO (Id MLCActivationLayer)
tanhShrinkLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' tanhShrinkLayerSelector

-- | Create a GELU activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ geluLayer@
geluLayer :: IO (Id MLCActivationLayer)
geluLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' geluLayerSelector

-- | Create a hardswish activation layer
--
-- Returns: A new activation layer
--
-- ObjC selector: @+ hardSwishLayer@
hardSwishLayer :: IO (Id MLCActivationLayer)
hardSwishLayer  =
  do
    cls' <- getRequiredClass "MLCActivationLayer"
    sendClassMessage cls' hardSwishLayerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector '[Id MLCActivationDescriptor] (Id MLCActivationLayer)
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @leakyReLULayerWithNegativeSlope:@
leakyReLULayerWithNegativeSlopeSelector :: Selector '[CFloat] (Id MLCActivationLayer)
leakyReLULayerWithNegativeSlopeSelector = mkSelector "leakyReLULayerWithNegativeSlope:"

-- | @Selector@ for @linearLayerWithScale:bias:@
linearLayerWithScale_biasSelector :: Selector '[CFloat, CFloat] (Id MLCActivationLayer)
linearLayerWithScale_biasSelector = mkSelector "linearLayerWithScale:bias:"

-- | @Selector@ for @softPlusLayerWithBeta:@
softPlusLayerWithBetaSelector :: Selector '[CFloat] (Id MLCActivationLayer)
softPlusLayerWithBetaSelector = mkSelector "softPlusLayerWithBeta:"

-- | @Selector@ for @eluLayerWithA:@
eluLayerWithASelector :: Selector '[CFloat] (Id MLCActivationLayer)
eluLayerWithASelector = mkSelector "eluLayerWithA:"

-- | @Selector@ for @relunLayerWithA:b:@
relunLayerWithA_bSelector :: Selector '[CFloat, CFloat] (Id MLCActivationLayer)
relunLayerWithA_bSelector = mkSelector "relunLayerWithA:b:"

-- | @Selector@ for @celuLayerWithA:@
celuLayerWithASelector :: Selector '[CFloat] (Id MLCActivationLayer)
celuLayerWithASelector = mkSelector "celuLayerWithA:"

-- | @Selector@ for @hardShrinkLayerWithA:@
hardShrinkLayerWithASelector :: Selector '[CFloat] (Id MLCActivationLayer)
hardShrinkLayerWithASelector = mkSelector "hardShrinkLayerWithA:"

-- | @Selector@ for @softShrinkLayerWithA:@
softShrinkLayerWithASelector :: Selector '[CFloat] (Id MLCActivationLayer)
softShrinkLayerWithASelector = mkSelector "softShrinkLayerWithA:"

-- | @Selector@ for @thresholdLayerWithThreshold:replacement:@
thresholdLayerWithThreshold_replacementSelector :: Selector '[CFloat, CFloat] (Id MLCActivationLayer)
thresholdLayerWithThreshold_replacementSelector = mkSelector "thresholdLayerWithThreshold:replacement:"

-- | @Selector@ for @clampLayerWithMinValue:maxValue:@
clampLayerWithMinValue_maxValueSelector :: Selector '[CFloat, CFloat] (Id MLCActivationLayer)
clampLayerWithMinValue_maxValueSelector = mkSelector "clampLayerWithMinValue:maxValue:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCActivationDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @reluLayer@
reluLayerSelector :: Selector '[] (Id MLCActivationLayer)
reluLayerSelector = mkSelector "reluLayer"

-- | @Selector@ for @relu6Layer@
relu6LayerSelector :: Selector '[] (Id MLCActivationLayer)
relu6LayerSelector = mkSelector "relu6Layer"

-- | @Selector@ for @leakyReLULayer@
leakyReLULayerSelector :: Selector '[] (Id MLCActivationLayer)
leakyReLULayerSelector = mkSelector "leakyReLULayer"

-- | @Selector@ for @sigmoidLayer@
sigmoidLayerSelector :: Selector '[] (Id MLCActivationLayer)
sigmoidLayerSelector = mkSelector "sigmoidLayer"

-- | @Selector@ for @hardSigmoidLayer@
hardSigmoidLayerSelector :: Selector '[] (Id MLCActivationLayer)
hardSigmoidLayerSelector = mkSelector "hardSigmoidLayer"

-- | @Selector@ for @tanhLayer@
tanhLayerSelector :: Selector '[] (Id MLCActivationLayer)
tanhLayerSelector = mkSelector "tanhLayer"

-- | @Selector@ for @absoluteLayer@
absoluteLayerSelector :: Selector '[] (Id MLCActivationLayer)
absoluteLayerSelector = mkSelector "absoluteLayer"

-- | @Selector@ for @softPlusLayer@
softPlusLayerSelector :: Selector '[] (Id MLCActivationLayer)
softPlusLayerSelector = mkSelector "softPlusLayer"

-- | @Selector@ for @softSignLayer@
softSignLayerSelector :: Selector '[] (Id MLCActivationLayer)
softSignLayerSelector = mkSelector "softSignLayer"

-- | @Selector@ for @eluLayer@
eluLayerSelector :: Selector '[] (Id MLCActivationLayer)
eluLayerSelector = mkSelector "eluLayer"

-- | @Selector@ for @logSigmoidLayer@
logSigmoidLayerSelector :: Selector '[] (Id MLCActivationLayer)
logSigmoidLayerSelector = mkSelector "logSigmoidLayer"

-- | @Selector@ for @seluLayer@
seluLayerSelector :: Selector '[] (Id MLCActivationLayer)
seluLayerSelector = mkSelector "seluLayer"

-- | @Selector@ for @celuLayer@
celuLayerSelector :: Selector '[] (Id MLCActivationLayer)
celuLayerSelector = mkSelector "celuLayer"

-- | @Selector@ for @hardShrinkLayer@
hardShrinkLayerSelector :: Selector '[] (Id MLCActivationLayer)
hardShrinkLayerSelector = mkSelector "hardShrinkLayer"

-- | @Selector@ for @softShrinkLayer@
softShrinkLayerSelector :: Selector '[] (Id MLCActivationLayer)
softShrinkLayerSelector = mkSelector "softShrinkLayer"

-- | @Selector@ for @tanhShrinkLayer@
tanhShrinkLayerSelector :: Selector '[] (Id MLCActivationLayer)
tanhShrinkLayerSelector = mkSelector "tanhShrinkLayer"

-- | @Selector@ for @geluLayer@
geluLayerSelector :: Selector '[] (Id MLCActivationLayer)
geluLayerSelector = mkSelector "geluLayer"

-- | @Selector@ for @hardSwishLayer@
hardSwishLayerSelector :: Selector '[] (Id MLCActivationLayer)
hardSwishLayerSelector = mkSelector "hardSwishLayer"


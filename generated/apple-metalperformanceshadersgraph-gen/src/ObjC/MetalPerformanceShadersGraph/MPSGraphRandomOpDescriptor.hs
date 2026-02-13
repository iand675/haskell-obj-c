{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that describes the random operation.
--
-- Generated bindings for @MPSGraphRandomOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphRandomOpDescriptor
  ( MPSGraphRandomOpDescriptor
  , IsMPSGraphRandomOpDescriptor(..)
  , descriptorWithDistribution_dataType
  , distribution
  , setDistribution
  , dataType
  , setDataType
  , min_
  , setMin
  , max_
  , setMax
  , minInteger
  , setMinInteger
  , maxInteger
  , setMaxInteger
  , mean
  , setMean
  , standardDeviation
  , setStandardDeviation
  , samplingMethod
  , setSamplingMethod
  , dataTypeSelector
  , descriptorWithDistribution_dataTypeSelector
  , distributionSelector
  , maxIntegerSelector
  , maxSelector
  , meanSelector
  , minIntegerSelector
  , minSelector
  , samplingMethodSelector
  , setDataTypeSelector
  , setDistributionSelector
  , setMaxIntegerSelector
  , setMaxSelector
  , setMeanSelector
  , setMinIntegerSelector
  , setMinSelector
  , setSamplingMethodSelector
  , setStandardDeviationSelector
  , standardDeviationSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSGraphRandomDistribution(MPSGraphRandomDistribution)
  , pattern MPSGraphRandomDistributionUniform
  , pattern MPSGraphRandomDistributionNormal
  , pattern MPSGraphRandomDistributionTruncatedNormal
  , MPSGraphRandomNormalSamplingMethod(MPSGraphRandomNormalSamplingMethod)
  , pattern MPSGraphRandomNormalSamplingInvCDF
  , pattern MPSGraphRandomNormalSamplingBoxMuller

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Class method to initialize a distribution descriptor.
--
-- ObjC selector: @+ descriptorWithDistribution:dataType:@
descriptorWithDistribution_dataType :: MPSGraphRandomDistribution -> MPSDataType -> IO (Id MPSGraphRandomOpDescriptor)
descriptorWithDistribution_dataType distribution dataType =
  do
    cls' <- getRequiredClass "MPSGraphRandomOpDescriptor"
    sendClassMessage cls' descriptorWithDistribution_dataTypeSelector distribution dataType

-- | The type of distribution to draw samples from. See MPSGraphRandomDistribution.
--
-- ObjC selector: @- distribution@
distribution :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO MPSGraphRandomDistribution
distribution mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor distributionSelector

-- | The type of distribution to draw samples from. See MPSGraphRandomDistribution.
--
-- ObjC selector: @- setDistribution:@
setDistribution :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> MPSGraphRandomDistribution -> IO ()
setDistribution mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setDistributionSelector value

-- | The data type of the generated result values.
--
-- When sampling from the uniform distribution, valid types are MPSDataTypeFloat16, MPSDataTypeFloat32, and MPSDataTypeInt32. When sampling from the normal or truncated normal distribution, valid types are  MPSDataTypeFloat16 and MPSDataTypeFloat32.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO MPSDataType
dataType mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor dataTypeSelector

-- | The data type of the generated result values.
--
-- When sampling from the uniform distribution, valid types are MPSDataTypeFloat16, MPSDataTypeFloat32, and MPSDataTypeInt32. When sampling from the normal or truncated normal distribution, valid types are  MPSDataTypeFloat16 and MPSDataTypeFloat32.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> MPSDataType -> IO ()
setDataType mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setDataTypeSelector value

-- | The lower range of the distribution.
--
-- This value is used for Uniform distributions with float data types and Truncated Normal disributions. Defaults to 0 for uniform distributions and -2 for normal distributions.
--
-- ObjC selector: @- min@
min_ :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CFloat
min_ mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor minSelector

-- | The lower range of the distribution.
--
-- This value is used for Uniform distributions with float data types and Truncated Normal disributions. Defaults to 0 for uniform distributions and -2 for normal distributions.
--
-- ObjC selector: @- setMin:@
setMin :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CFloat -> IO ()
setMin mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setMinSelector value

-- | The upper range of the distribution.
--
-- This value is used for Uniform distributions with float data types and Truncated Normal disributions. Defaults to 1 for uniform distributions and 2 for normal distributions.
--
-- ObjC selector: @- max@
max_ :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CFloat
max_ mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor maxSelector

-- | The upper range of the distribution.
--
-- This value is used for Uniform distributions with float data types and Truncated Normal disributions. Defaults to 1 for uniform distributions and 2 for normal distributions.
--
-- ObjC selector: @- setMax:@
setMax :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CFloat -> IO ()
setMax mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setMaxSelector value

-- | The lower range of the distribution.
--
-- This value is used for Uniform with integer data types Defaults to 0.
--
-- ObjC selector: @- minInteger@
minInteger :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CLong
minInteger mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor minIntegerSelector

-- | The lower range of the distribution.
--
-- This value is used for Uniform with integer data types Defaults to 0.
--
-- ObjC selector: @- setMinInteger:@
setMinInteger :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CLong -> IO ()
setMinInteger mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setMinIntegerSelector value

-- | The upper range of the distribution.
--
-- This value is used for Uniform with integer data types Defaults to INT32_MAX for uniform distributions and 0 for normal distributions.
--
-- ObjC selector: @- maxInteger@
maxInteger :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CLong
maxInteger mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor maxIntegerSelector

-- | The upper range of the distribution.
--
-- This value is used for Uniform with integer data types Defaults to INT32_MAX for uniform distributions and 0 for normal distributions.
--
-- ObjC selector: @- setMaxInteger:@
setMaxInteger :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CLong -> IO ()
setMaxInteger mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setMaxIntegerSelector value

-- | The mean of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. Defaults to 0.
--
-- ObjC selector: @- mean@
mean :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CFloat
mean mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor meanSelector

-- | The mean of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. Defaults to 0.
--
-- ObjC selector: @- setMean:@
setMean :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CFloat -> IO ()
setMean mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setMeanSelector value

-- | The standard deviation of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. For Truncated Normal distribution this defines the standard deviation parameter of the underlying Normal distribution, that is the width of the Gaussian, not the true standard deviation of the truncated distribution which typically differs from the standard deviation of the  original Normal distribution.  Defaults to 0 for uniform distributions and 1 for normal distributions.
--
-- ObjC selector: @- standardDeviation@
standardDeviation :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO CFloat
standardDeviation mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor standardDeviationSelector

-- | The standard deviation of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. For Truncated Normal distribution this defines the standard deviation parameter of the underlying Normal distribution, that is the width of the Gaussian, not the true standard deviation of the truncated distribution which typically differs from the standard deviation of the  original Normal distribution.  Defaults to 0 for uniform distributions and 1 for normal distributions.
--
-- ObjC selector: @- setStandardDeviation:@
setStandardDeviation :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> CFloat -> IO ()
setStandardDeviation mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setStandardDeviationSelector value

-- | The sampling method of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. See MPSGraphRandomNormalSamplingMethod. Defaults to MPSGraphRandomNormalSamplingInvCDF.
--
-- ObjC selector: @- samplingMethod@
samplingMethod :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> IO MPSGraphRandomNormalSamplingMethod
samplingMethod mpsGraphRandomOpDescriptor =
  sendMessage mpsGraphRandomOpDescriptor samplingMethodSelector

-- | The sampling method of the distribution.
--
-- This value is used for Normal and Truncated Normal disributions. See MPSGraphRandomNormalSamplingMethod. Defaults to MPSGraphRandomNormalSamplingInvCDF.
--
-- ObjC selector: @- setSamplingMethod:@
setSamplingMethod :: IsMPSGraphRandomOpDescriptor mpsGraphRandomOpDescriptor => mpsGraphRandomOpDescriptor -> MPSGraphRandomNormalSamplingMethod -> IO ()
setSamplingMethod mpsGraphRandomOpDescriptor value =
  sendMessage mpsGraphRandomOpDescriptor setSamplingMethodSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithDistribution:dataType:@
descriptorWithDistribution_dataTypeSelector :: Selector '[MPSGraphRandomDistribution, MPSDataType] (Id MPSGraphRandomOpDescriptor)
descriptorWithDistribution_dataTypeSelector = mkSelector "descriptorWithDistribution:dataType:"

-- | @Selector@ for @distribution@
distributionSelector :: Selector '[] MPSGraphRandomDistribution
distributionSelector = mkSelector "distribution"

-- | @Selector@ for @setDistribution:@
setDistributionSelector :: Selector '[MPSGraphRandomDistribution] ()
setDistributionSelector = mkSelector "setDistribution:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MPSDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @min@
minSelector :: Selector '[] CFloat
minSelector = mkSelector "min"

-- | @Selector@ for @setMin:@
setMinSelector :: Selector '[CFloat] ()
setMinSelector = mkSelector "setMin:"

-- | @Selector@ for @max@
maxSelector :: Selector '[] CFloat
maxSelector = mkSelector "max"

-- | @Selector@ for @setMax:@
setMaxSelector :: Selector '[CFloat] ()
setMaxSelector = mkSelector "setMax:"

-- | @Selector@ for @minInteger@
minIntegerSelector :: Selector '[] CLong
minIntegerSelector = mkSelector "minInteger"

-- | @Selector@ for @setMinInteger:@
setMinIntegerSelector :: Selector '[CLong] ()
setMinIntegerSelector = mkSelector "setMinInteger:"

-- | @Selector@ for @maxInteger@
maxIntegerSelector :: Selector '[] CLong
maxIntegerSelector = mkSelector "maxInteger"

-- | @Selector@ for @setMaxInteger:@
setMaxIntegerSelector :: Selector '[CLong] ()
setMaxIntegerSelector = mkSelector "setMaxInteger:"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] CFloat
meanSelector = mkSelector "mean"

-- | @Selector@ for @setMean:@
setMeanSelector :: Selector '[CFloat] ()
setMeanSelector = mkSelector "setMean:"

-- | @Selector@ for @standardDeviation@
standardDeviationSelector :: Selector '[] CFloat
standardDeviationSelector = mkSelector "standardDeviation"

-- | @Selector@ for @setStandardDeviation:@
setStandardDeviationSelector :: Selector '[CFloat] ()
setStandardDeviationSelector = mkSelector "setStandardDeviation:"

-- | @Selector@ for @samplingMethod@
samplingMethodSelector :: Selector '[] MPSGraphRandomNormalSamplingMethod
samplingMethodSelector = mkSelector "samplingMethod"

-- | @Selector@ for @setSamplingMethod:@
setSamplingMethodSelector :: Selector '[MPSGraphRandomNormalSamplingMethod] ()
setSamplingMethodSelector = mkSelector "setSamplingMethod:"


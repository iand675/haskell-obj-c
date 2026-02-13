{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a fast Fourier transform (FFT) operation.
--
-- Use this descriptor with ``MPSGraph/fastFourierTransformWithTensor:axes:descriptor:name:``, ``MPSGraph/realToHermiteanFFTWithTensor:axesTensor:descriptor:name:``, and ``MPSGraph/HermiteanToRealFFTWithTensor:axesTensor:descriptor:name:`` methods.
--
-- Generated bindings for @MPSGraphFFTDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphFFTDescriptor
  ( MPSGraphFFTDescriptor
  , IsMPSGraphFFTDescriptor(..)
  , descriptor
  , inverse
  , setInverse
  , scalingMode
  , setScalingMode
  , roundToOddHermitean
  , setRoundToOddHermitean
  , descriptorSelector
  , inverseSelector
  , roundToOddHermiteanSelector
  , scalingModeSelector
  , setInverseSelector
  , setRoundToOddHermiteanSelector
  , setScalingModeSelector

  -- * Enum types
  , MPSGraphFFTScalingMode(MPSGraphFFTScalingMode)
  , pattern MPSGraphFFTScalingModeNone
  , pattern MPSGraphFFTScalingModeSize
  , pattern MPSGraphFFTScalingModeUnitary

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a fast Fourier transform descriptor with default parameter values.
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MPSGraphFFTDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MPSGraphFFTDescriptor"
    sendClassMessage cls' descriptorSelector

-- | A Boolean-valued parameter that defines the phase factor sign for Fourier transforms.
--
-- When set to @YES@ graph uses the positive phase factor: @exp(+i 2Pi mu nu / n)@, when computing the (inverse) Fourier transform. Otherwise MPSGraph uses the negative phase factor: @exp(-i 2Pi mu nu / n)@, when computing the Fourier transform. Default value: @NO@.
--
-- ObjC selector: @- inverse@
inverse :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> IO Bool
inverse mpsGraphFFTDescriptor =
  sendMessage mpsGraphFFTDescriptor inverseSelector

-- | A Boolean-valued parameter that defines the phase factor sign for Fourier transforms.
--
-- When set to @YES@ graph uses the positive phase factor: @exp(+i 2Pi mu nu / n)@, when computing the (inverse) Fourier transform. Otherwise MPSGraph uses the negative phase factor: @exp(-i 2Pi mu nu / n)@, when computing the Fourier transform. Default value: @NO@.
--
-- ObjC selector: @- setInverse:@
setInverse :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> Bool -> IO ()
setInverse mpsGraphFFTDescriptor value =
  sendMessage mpsGraphFFTDescriptor setInverseSelector value

-- | The scaling mode of the fast fourier transform (FFT) operation.
--
-- Note that the scaling mode is independent from the phase factor. Default value: @MPSGraphFFTScalingModeNone@.
--
-- ObjC selector: @- scalingMode@
scalingMode :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> IO MPSGraphFFTScalingMode
scalingMode mpsGraphFFTDescriptor =
  sendMessage mpsGraphFFTDescriptor scalingModeSelector

-- | The scaling mode of the fast fourier transform (FFT) operation.
--
-- Note that the scaling mode is independent from the phase factor. Default value: @MPSGraphFFTScalingModeNone@.
--
-- ObjC selector: @- setScalingMode:@
setScalingMode :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> MPSGraphFFTScalingMode -> IO ()
setScalingMode mpsGraphFFTDescriptor value =
  sendMessage mpsGraphFFTDescriptor setScalingModeSelector value

-- | A parameter which controls how graph rounds the output tensor size for a Hermitean-to-real Fourier transform.
--
-- If set to @YES@ then MPSGraph rounds the last output dimension of the result tensor in ``MPSGraph/HermiteanToRealFFTWithTensor:axesTensor:descriptor:name:`` to an odd value. Has no effect in the other Fourier transform operations. Default value: @NO@.
--
-- ObjC selector: @- roundToOddHermitean@
roundToOddHermitean :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> IO Bool
roundToOddHermitean mpsGraphFFTDescriptor =
  sendMessage mpsGraphFFTDescriptor roundToOddHermiteanSelector

-- | A parameter which controls how graph rounds the output tensor size for a Hermitean-to-real Fourier transform.
--
-- If set to @YES@ then MPSGraph rounds the last output dimension of the result tensor in ``MPSGraph/HermiteanToRealFFTWithTensor:axesTensor:descriptor:name:`` to an odd value. Has no effect in the other Fourier transform operations. Default value: @NO@.
--
-- ObjC selector: @- setRoundToOddHermitean:@
setRoundToOddHermitean :: IsMPSGraphFFTDescriptor mpsGraphFFTDescriptor => mpsGraphFFTDescriptor -> Bool -> IO ()
setRoundToOddHermitean mpsGraphFFTDescriptor value =
  sendMessage mpsGraphFFTDescriptor setRoundToOddHermiteanSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MPSGraphFFTDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @inverse@
inverseSelector :: Selector '[] Bool
inverseSelector = mkSelector "inverse"

-- | @Selector@ for @setInverse:@
setInverseSelector :: Selector '[Bool] ()
setInverseSelector = mkSelector "setInverse:"

-- | @Selector@ for @scalingMode@
scalingModeSelector :: Selector '[] MPSGraphFFTScalingMode
scalingModeSelector = mkSelector "scalingMode"

-- | @Selector@ for @setScalingMode:@
setScalingModeSelector :: Selector '[MPSGraphFFTScalingMode] ()
setScalingModeSelector = mkSelector "setScalingMode:"

-- | @Selector@ for @roundToOddHermitean@
roundToOddHermiteanSelector :: Selector '[] Bool
roundToOddHermiteanSelector = mkSelector "roundToOddHermitean"

-- | @Selector@ for @setRoundToOddHermitean:@
setRoundToOddHermiteanSelector :: Selector '[Bool] ()
setRoundToOddHermiteanSelector = mkSelector "setRoundToOddHermitean:"


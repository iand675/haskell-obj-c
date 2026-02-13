{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLSamplerDescriptor
--
-- A mutable descriptor used to configure a sampler.  When complete, this can be used to create an immutable MTLSamplerState.
--
-- Generated bindings for @MTLSamplerDescriptor@.
module ObjC.Metal.MTLSamplerDescriptor
  ( MTLSamplerDescriptor
  , IsMTLSamplerDescriptor(..)
  , minFilter
  , setMinFilter
  , magFilter
  , setMagFilter
  , mipFilter
  , setMipFilter
  , maxAnisotropy
  , setMaxAnisotropy
  , sAddressMode
  , setSAddressMode
  , tAddressMode
  , setTAddressMode
  , rAddressMode
  , setRAddressMode
  , borderColor
  , setBorderColor
  , reductionMode
  , setReductionMode
  , normalizedCoordinates
  , setNormalizedCoordinates
  , lodMinClamp
  , setLodMinClamp
  , lodMaxClamp
  , setLodMaxClamp
  , lodAverage
  , setLodAverage
  , lodBias
  , setLodBias
  , compareFunction
  , setCompareFunction
  , supportArgumentBuffers
  , setSupportArgumentBuffers
  , label
  , setLabel
  , borderColorSelector
  , compareFunctionSelector
  , labelSelector
  , lodAverageSelector
  , lodBiasSelector
  , lodMaxClampSelector
  , lodMinClampSelector
  , magFilterSelector
  , maxAnisotropySelector
  , minFilterSelector
  , mipFilterSelector
  , normalizedCoordinatesSelector
  , rAddressModeSelector
  , reductionModeSelector
  , sAddressModeSelector
  , setBorderColorSelector
  , setCompareFunctionSelector
  , setLabelSelector
  , setLodAverageSelector
  , setLodBiasSelector
  , setLodMaxClampSelector
  , setLodMinClampSelector
  , setMagFilterSelector
  , setMaxAnisotropySelector
  , setMinFilterSelector
  , setMipFilterSelector
  , setNormalizedCoordinatesSelector
  , setRAddressModeSelector
  , setReductionModeSelector
  , setSAddressModeSelector
  , setSupportArgumentBuffersSelector
  , setTAddressModeSelector
  , supportArgumentBuffersSelector
  , tAddressModeSelector

  -- * Enum types
  , MTLCompareFunction(MTLCompareFunction)
  , pattern MTLCompareFunctionNever
  , pattern MTLCompareFunctionLess
  , pattern MTLCompareFunctionEqual
  , pattern MTLCompareFunctionLessEqual
  , pattern MTLCompareFunctionGreater
  , pattern MTLCompareFunctionNotEqual
  , pattern MTLCompareFunctionGreaterEqual
  , pattern MTLCompareFunctionAlways
  , MTLSamplerAddressMode(MTLSamplerAddressMode)
  , pattern MTLSamplerAddressModeClampToEdge
  , pattern MTLSamplerAddressModeMirrorClampToEdge
  , pattern MTLSamplerAddressModeRepeat
  , pattern MTLSamplerAddressModeMirrorRepeat
  , pattern MTLSamplerAddressModeClampToZero
  , pattern MTLSamplerAddressModeClampToBorderColor
  , MTLSamplerBorderColor(MTLSamplerBorderColor)
  , pattern MTLSamplerBorderColorTransparentBlack
  , pattern MTLSamplerBorderColorOpaqueBlack
  , pattern MTLSamplerBorderColorOpaqueWhite
  , MTLSamplerMinMagFilter(MTLSamplerMinMagFilter)
  , pattern MTLSamplerMinMagFilterNearest
  , pattern MTLSamplerMinMagFilterLinear
  , MTLSamplerMipFilter(MTLSamplerMipFilter)
  , pattern MTLSamplerMipFilterNotMipmapped
  , pattern MTLSamplerMipFilterNearest
  , pattern MTLSamplerMipFilterLinear
  , MTLSamplerReductionMode(MTLSamplerReductionMode)
  , pattern MTLSamplerReductionModeWeightedAverage
  , pattern MTLSamplerReductionModeMinimum
  , pattern MTLSamplerReductionModeMaximum

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | minFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is larger than a pixel (minification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- minFilter@
minFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerMinMagFilter
minFilter mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor minFilterSelector

-- | minFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is larger than a pixel (minification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- setMinFilter:@
setMinFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMinMagFilter -> IO ()
setMinFilter mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setMinFilterSelector value

-- | magFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is smaller than a pixel (magnification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- magFilter@
magFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerMinMagFilter
magFilter mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor magFilterSelector

-- | magFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is smaller than a pixel (magnification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- setMagFilter:@
setMagFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMinMagFilter -> IO ()
setMagFilter mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setMagFilterSelector value

-- | mipFilter
--
-- Filter options for filtering between two mipmap levels.
--
-- The default value is MTLSamplerMipFilterNotMipmapped
--
-- ObjC selector: @- mipFilter@
mipFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerMipFilter
mipFilter mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor mipFilterSelector

-- | mipFilter
--
-- Filter options for filtering between two mipmap levels.
--
-- The default value is MTLSamplerMipFilterNotMipmapped
--
-- ObjC selector: @- setMipFilter:@
setMipFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMipFilter -> IO ()
setMipFilter mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setMipFilterSelector value

-- | maxAnisotropy
--
-- The number of samples that can be taken to improve quality of sample footprints that are anisotropic.
--
-- The default value is 1.
--
-- ObjC selector: @- maxAnisotropy@
maxAnisotropy :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CULong
maxAnisotropy mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor maxAnisotropySelector

-- | maxAnisotropy
--
-- The number of samples that can be taken to improve quality of sample footprints that are anisotropic.
--
-- The default value is 1.
--
-- ObjC selector: @- setMaxAnisotropy:@
setMaxAnisotropy :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CULong -> IO ()
setMaxAnisotropy mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setMaxAnisotropySelector value

-- | sAddressMode
--
-- Set the wrap mode for the S texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- sAddressMode@
sAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
sAddressMode mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor sAddressModeSelector

-- | sAddressMode
--
-- Set the wrap mode for the S texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setSAddressMode:@
setSAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setSAddressMode mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setSAddressModeSelector value

-- | tAddressMode
--
-- Set the wrap mode for the T texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- tAddressMode@
tAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
tAddressMode mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor tAddressModeSelector

-- | tAddressMode
--
-- Set the wrap mode for the T texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setTAddressMode:@
setTAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setTAddressMode mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setTAddressModeSelector value

-- | rAddressMode
--
-- Set the wrap mode for the R texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- rAddressMode@
rAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
rAddressMode mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor rAddressModeSelector

-- | rAddressMode
--
-- Set the wrap mode for the R texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setRAddressMode:@
setRAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setRAddressMode mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setRAddressModeSelector value

-- | borderColor
--
-- Set the color for the MTLSamplerAddressMode to one of the predefined in the MTLSamplerBorderColor enum.
--
-- ObjC selector: @- borderColor@
borderColor :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerBorderColor
borderColor mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor borderColorSelector

-- | borderColor
--
-- Set the color for the MTLSamplerAddressMode to one of the predefined in the MTLSamplerBorderColor enum.
--
-- ObjC selector: @- setBorderColor:@
setBorderColor :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerBorderColor -> IO ()
setBorderColor mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setBorderColorSelector value

-- | Sets the reduction mode for filtering contributing samples.
--
-- The property's default value is ``MTLSamplerReductionModeWeightedAverage``. The sampler ignores this property if any of the following property values are equal to a specific value:  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNotMipmapped``.  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNearest``.  - The sampler's ``minFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.  - The sampler's ``magFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.
--
-- ObjC selector: @- reductionMode@
reductionMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerReductionMode
reductionMode mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor reductionModeSelector

-- | Sets the reduction mode for filtering contributing samples.
--
-- The property's default value is ``MTLSamplerReductionModeWeightedAverage``. The sampler ignores this property if any of the following property values are equal to a specific value:  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNotMipmapped``.  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNearest``.  - The sampler's ``minFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.  - The sampler's ``magFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.
--
-- ObjC selector: @- setReductionMode:@
setReductionMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerReductionMode -> IO ()
setReductionMode mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setReductionModeSelector value

-- | normalizedCoordinates.
--
-- If YES, texture coordates are from 0 to 1.  If NO, texture coordinates are 0..width, 0..height.
--
-- normalizedCoordinates defaults to YES.  Non-normalized coordinates should only be used with 1D and 2D textures with the ClampToEdge wrap mode, otherwise the results of sampling are undefined.
--
-- ObjC selector: @- normalizedCoordinates@
normalizedCoordinates :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
normalizedCoordinates mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor normalizedCoordinatesSelector

-- | normalizedCoordinates.
--
-- If YES, texture coordates are from 0 to 1.  If NO, texture coordinates are 0..width, 0..height.
--
-- normalizedCoordinates defaults to YES.  Non-normalized coordinates should only be used with 1D and 2D textures with the ClampToEdge wrap mode, otherwise the results of sampling are undefined.
--
-- ObjC selector: @- setNormalizedCoordinates:@
setNormalizedCoordinates :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setNormalizedCoordinates mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setNormalizedCoordinatesSelector value

-- | lodMinClamp
--
-- The minimum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMinClamp is 0.0.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- lodMinClamp@
lodMinClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodMinClamp mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor lodMinClampSelector

-- | lodMinClamp
--
-- The minimum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMinClamp is 0.0.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- setLodMinClamp:@
setLodMinClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodMinClamp mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setLodMinClampSelector value

-- | lodMaxClamp
--
-- The maximum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMaxClamp is FLT_MAX.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- lodMaxClamp@
lodMaxClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodMaxClamp mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor lodMaxClampSelector

-- | lodMaxClamp
--
-- The maximum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMaxClamp is FLT_MAX.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- setLodMaxClamp:@
setLodMaxClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodMaxClamp mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setLodMaxClampSelector value

-- | lodAverage
--
-- If YES, an average level of detail will be used when sampling from a texture. If NO, no averaging is performed.
--
-- lodAverage defaults to NO. This option is a performance hint. An implementation is free to ignore this property.
--
-- ObjC selector: @- lodAverage@
lodAverage :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
lodAverage mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor lodAverageSelector

-- | lodAverage
--
-- If YES, an average level of detail will be used when sampling from a texture. If NO, no averaging is performed.
--
-- lodAverage defaults to NO. This option is a performance hint. An implementation is free to ignore this property.
--
-- ObjC selector: @- setLodAverage:@
setLodAverage :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setLodAverage mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setLodAverageSelector value

-- | Sets the level-of-detail (lod) bias when sampling from a texture.
--
-- The property's default value is @0.0f@. The precision format is @S4.6@, and the range is @[-16.0, 15.999]@.
--
-- ObjC selector: @- lodBias@
lodBias :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodBias mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor lodBiasSelector

-- | Sets the level-of-detail (lod) bias when sampling from a texture.
--
-- The property's default value is @0.0f@. The precision format is @S4.6@, and the range is @[-16.0, 15.999]@.
--
-- ObjC selector: @- setLodBias:@
setLodBias :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodBias mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setLodBiasSelector value

-- | compareFunction
--
-- Set the comparison function used when sampling shadow maps. The default value is MTLCompareFunctionNever.
--
-- ObjC selector: @- compareFunction@
compareFunction :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLCompareFunction
compareFunction mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor compareFunctionSelector

-- | compareFunction
--
-- Set the comparison function used when sampling shadow maps. The default value is MTLCompareFunctionNever.
--
-- ObjC selector: @- setCompareFunction:@
setCompareFunction :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLCompareFunction -> IO ()
setCompareFunction mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setCompareFunctionSelector value

-- | supportArgumentBuffers
--
-- true if the sampler can be used inside an argument buffer
--
-- ObjC selector: @- supportArgumentBuffers@
supportArgumentBuffers :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
supportArgumentBuffers mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor supportArgumentBuffersSelector

-- | supportArgumentBuffers
--
-- true if the sampler can be used inside an argument buffer
--
-- ObjC selector: @- setSupportArgumentBuffers:@
setSupportArgumentBuffers :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setSupportArgumentBuffers mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setSupportArgumentBuffersSelector value

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- label@
label :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO (Id NSString)
label mtlSamplerDescriptor =
  sendMessage mtlSamplerDescriptor labelSelector

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLSamplerDescriptor mtlSamplerDescriptor, IsNSString value) => mtlSamplerDescriptor -> value -> IO ()
setLabel mtlSamplerDescriptor value =
  sendMessage mtlSamplerDescriptor setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minFilter@
minFilterSelector :: Selector '[] MTLSamplerMinMagFilter
minFilterSelector = mkSelector "minFilter"

-- | @Selector@ for @setMinFilter:@
setMinFilterSelector :: Selector '[MTLSamplerMinMagFilter] ()
setMinFilterSelector = mkSelector "setMinFilter:"

-- | @Selector@ for @magFilter@
magFilterSelector :: Selector '[] MTLSamplerMinMagFilter
magFilterSelector = mkSelector "magFilter"

-- | @Selector@ for @setMagFilter:@
setMagFilterSelector :: Selector '[MTLSamplerMinMagFilter] ()
setMagFilterSelector = mkSelector "setMagFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector '[] MTLSamplerMipFilter
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector '[MTLSamplerMipFilter] ()
setMipFilterSelector = mkSelector "setMipFilter:"

-- | @Selector@ for @maxAnisotropy@
maxAnisotropySelector :: Selector '[] CULong
maxAnisotropySelector = mkSelector "maxAnisotropy"

-- | @Selector@ for @setMaxAnisotropy:@
setMaxAnisotropySelector :: Selector '[CULong] ()
setMaxAnisotropySelector = mkSelector "setMaxAnisotropy:"

-- | @Selector@ for @sAddressMode@
sAddressModeSelector :: Selector '[] MTLSamplerAddressMode
sAddressModeSelector = mkSelector "sAddressMode"

-- | @Selector@ for @setSAddressMode:@
setSAddressModeSelector :: Selector '[MTLSamplerAddressMode] ()
setSAddressModeSelector = mkSelector "setSAddressMode:"

-- | @Selector@ for @tAddressMode@
tAddressModeSelector :: Selector '[] MTLSamplerAddressMode
tAddressModeSelector = mkSelector "tAddressMode"

-- | @Selector@ for @setTAddressMode:@
setTAddressModeSelector :: Selector '[MTLSamplerAddressMode] ()
setTAddressModeSelector = mkSelector "setTAddressMode:"

-- | @Selector@ for @rAddressMode@
rAddressModeSelector :: Selector '[] MTLSamplerAddressMode
rAddressModeSelector = mkSelector "rAddressMode"

-- | @Selector@ for @setRAddressMode:@
setRAddressModeSelector :: Selector '[MTLSamplerAddressMode] ()
setRAddressModeSelector = mkSelector "setRAddressMode:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] MTLSamplerBorderColor
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[MTLSamplerBorderColor] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @reductionMode@
reductionModeSelector :: Selector '[] MTLSamplerReductionMode
reductionModeSelector = mkSelector "reductionMode"

-- | @Selector@ for @setReductionMode:@
setReductionModeSelector :: Selector '[MTLSamplerReductionMode] ()
setReductionModeSelector = mkSelector "setReductionMode:"

-- | @Selector@ for @normalizedCoordinates@
normalizedCoordinatesSelector :: Selector '[] Bool
normalizedCoordinatesSelector = mkSelector "normalizedCoordinates"

-- | @Selector@ for @setNormalizedCoordinates:@
setNormalizedCoordinatesSelector :: Selector '[Bool] ()
setNormalizedCoordinatesSelector = mkSelector "setNormalizedCoordinates:"

-- | @Selector@ for @lodMinClamp@
lodMinClampSelector :: Selector '[] CFloat
lodMinClampSelector = mkSelector "lodMinClamp"

-- | @Selector@ for @setLodMinClamp:@
setLodMinClampSelector :: Selector '[CFloat] ()
setLodMinClampSelector = mkSelector "setLodMinClamp:"

-- | @Selector@ for @lodMaxClamp@
lodMaxClampSelector :: Selector '[] CFloat
lodMaxClampSelector = mkSelector "lodMaxClamp"

-- | @Selector@ for @setLodMaxClamp:@
setLodMaxClampSelector :: Selector '[CFloat] ()
setLodMaxClampSelector = mkSelector "setLodMaxClamp:"

-- | @Selector@ for @lodAverage@
lodAverageSelector :: Selector '[] Bool
lodAverageSelector = mkSelector "lodAverage"

-- | @Selector@ for @setLodAverage:@
setLodAverageSelector :: Selector '[Bool] ()
setLodAverageSelector = mkSelector "setLodAverage:"

-- | @Selector@ for @lodBias@
lodBiasSelector :: Selector '[] CFloat
lodBiasSelector = mkSelector "lodBias"

-- | @Selector@ for @setLodBias:@
setLodBiasSelector :: Selector '[CFloat] ()
setLodBiasSelector = mkSelector "setLodBias:"

-- | @Selector@ for @compareFunction@
compareFunctionSelector :: Selector '[] MTLCompareFunction
compareFunctionSelector = mkSelector "compareFunction"

-- | @Selector@ for @setCompareFunction:@
setCompareFunctionSelector :: Selector '[MTLCompareFunction] ()
setCompareFunctionSelector = mkSelector "setCompareFunction:"

-- | @Selector@ for @supportArgumentBuffers@
supportArgumentBuffersSelector :: Selector '[] Bool
supportArgumentBuffersSelector = mkSelector "supportArgumentBuffers"

-- | @Selector@ for @setSupportArgumentBuffers:@
setSupportArgumentBuffersSelector :: Selector '[Bool] ()
setSupportArgumentBuffersSelector = mkSelector "setSupportArgumentBuffers:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"


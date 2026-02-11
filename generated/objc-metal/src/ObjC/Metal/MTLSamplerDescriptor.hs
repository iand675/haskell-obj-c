{-# LANGUAGE PatternSynonyms #-}
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
  , minFilterSelector
  , setMinFilterSelector
  , magFilterSelector
  , setMagFilterSelector
  , mipFilterSelector
  , setMipFilterSelector
  , maxAnisotropySelector
  , setMaxAnisotropySelector
  , sAddressModeSelector
  , setSAddressModeSelector
  , tAddressModeSelector
  , setTAddressModeSelector
  , rAddressModeSelector
  , setRAddressModeSelector
  , borderColorSelector
  , setBorderColorSelector
  , reductionModeSelector
  , setReductionModeSelector
  , normalizedCoordinatesSelector
  , setNormalizedCoordinatesSelector
  , lodMinClampSelector
  , setLodMinClampSelector
  , lodMaxClampSelector
  , setLodMaxClampSelector
  , lodAverageSelector
  , setLodAverageSelector
  , lodBiasSelector
  , setLodBiasSelector
  , compareFunctionSelector
  , setCompareFunctionSelector
  , supportArgumentBuffersSelector
  , setSupportArgumentBuffersSelector
  , labelSelector
  , setLabelSelector

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
minFilter mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerMinMagFilter) $ sendMsg mtlSamplerDescriptor (mkSelector "minFilter") retCULong []

-- | minFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is larger than a pixel (minification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- setMinFilter:@
setMinFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMinMagFilter -> IO ()
setMinFilter mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setMinFilter:") retVoid [argCULong (coerce value)]

-- | magFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is smaller than a pixel (magnification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- magFilter@
magFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerMinMagFilter
magFilter mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerMinMagFilter) $ sendMsg mtlSamplerDescriptor (mkSelector "magFilter") retCULong []

-- | magFilter
--
-- Filter option for combining texels within a mipmap level the sample footprint is smaller than a pixel (magnification).
--
-- The default value is MTLSamplerMinMagFilterNearest.
--
-- ObjC selector: @- setMagFilter:@
setMagFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMinMagFilter -> IO ()
setMagFilter mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setMagFilter:") retVoid [argCULong (coerce value)]

-- | mipFilter
--
-- Filter options for filtering between two mipmap levels.
--
-- The default value is MTLSamplerMipFilterNotMipmapped
--
-- ObjC selector: @- mipFilter@
mipFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerMipFilter
mipFilter mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerMipFilter) $ sendMsg mtlSamplerDescriptor (mkSelector "mipFilter") retCULong []

-- | mipFilter
--
-- Filter options for filtering between two mipmap levels.
--
-- The default value is MTLSamplerMipFilterNotMipmapped
--
-- ObjC selector: @- setMipFilter:@
setMipFilter :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerMipFilter -> IO ()
setMipFilter mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setMipFilter:") retVoid [argCULong (coerce value)]

-- | maxAnisotropy
--
-- The number of samples that can be taken to improve quality of sample footprints that are anisotropic.
--
-- The default value is 1.
--
-- ObjC selector: @- maxAnisotropy@
maxAnisotropy :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CULong
maxAnisotropy mtlSamplerDescriptor  =
  sendMsg mtlSamplerDescriptor (mkSelector "maxAnisotropy") retCULong []

-- | maxAnisotropy
--
-- The number of samples that can be taken to improve quality of sample footprints that are anisotropic.
--
-- The default value is 1.
--
-- ObjC selector: @- setMaxAnisotropy:@
setMaxAnisotropy :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CULong -> IO ()
setMaxAnisotropy mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setMaxAnisotropy:") retVoid [argCULong (fromIntegral value)]

-- | sAddressMode
--
-- Set the wrap mode for the S texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- sAddressMode@
sAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
sAddressMode mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerAddressMode) $ sendMsg mtlSamplerDescriptor (mkSelector "sAddressMode") retCULong []

-- | sAddressMode
--
-- Set the wrap mode for the S texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setSAddressMode:@
setSAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setSAddressMode mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setSAddressMode:") retVoid [argCULong (coerce value)]

-- | tAddressMode
--
-- Set the wrap mode for the T texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- tAddressMode@
tAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
tAddressMode mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerAddressMode) $ sendMsg mtlSamplerDescriptor (mkSelector "tAddressMode") retCULong []

-- | tAddressMode
--
-- Set the wrap mode for the T texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setTAddressMode:@
setTAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setTAddressMode mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setTAddressMode:") retVoid [argCULong (coerce value)]

-- | rAddressMode
--
-- Set the wrap mode for the R texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- rAddressMode@
rAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerAddressMode
rAddressMode mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerAddressMode) $ sendMsg mtlSamplerDescriptor (mkSelector "rAddressMode") retCULong []

-- | rAddressMode
--
-- Set the wrap mode for the R texture coordinate.  The default value is MTLSamplerAddressModeClampToEdge.
--
-- ObjC selector: @- setRAddressMode:@
setRAddressMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerAddressMode -> IO ()
setRAddressMode mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setRAddressMode:") retVoid [argCULong (coerce value)]

-- | borderColor
--
-- Set the color for the MTLSamplerAddressMode to one of the predefined in the MTLSamplerBorderColor enum.
--
-- ObjC selector: @- borderColor@
borderColor :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerBorderColor
borderColor mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerBorderColor) $ sendMsg mtlSamplerDescriptor (mkSelector "borderColor") retCULong []

-- | borderColor
--
-- Set the color for the MTLSamplerAddressMode to one of the predefined in the MTLSamplerBorderColor enum.
--
-- ObjC selector: @- setBorderColor:@
setBorderColor :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerBorderColor -> IO ()
setBorderColor mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setBorderColor:") retVoid [argCULong (coerce value)]

-- | Sets the reduction mode for filtering contributing samples.
--
-- The property's default value is ``MTLSamplerReductionModeWeightedAverage``. The sampler ignores this property if any of the following property values are equal to a specific value:  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNotMipmapped``.  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNearest``.  - The sampler's ``minFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.  - The sampler's ``magFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.
--
-- ObjC selector: @- reductionMode@
reductionMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLSamplerReductionMode
reductionMode mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLSamplerReductionMode) $ sendMsg mtlSamplerDescriptor (mkSelector "reductionMode") retCULong []

-- | Sets the reduction mode for filtering contributing samples.
--
-- The property's default value is ``MTLSamplerReductionModeWeightedAverage``. The sampler ignores this property if any of the following property values are equal to a specific value:  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNotMipmapped``.  - The sampler's ``mipFilter`` property is equal to ``MTLSamplerMipFilterNearest``.  - The sampler's ``minFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.  - The sampler's ``magFilter`` property is equal to ``MTLSamplerMinMagFilterNearest``.
--
-- ObjC selector: @- setReductionMode:@
setReductionMode :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLSamplerReductionMode -> IO ()
setReductionMode mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setReductionMode:") retVoid [argCULong (coerce value)]

-- | normalizedCoordinates.
--
-- If YES, texture coordates are from 0 to 1.  If NO, texture coordinates are 0..width, 0..height.
--
-- normalizedCoordinates defaults to YES.  Non-normalized coordinates should only be used with 1D and 2D textures with the ClampToEdge wrap mode, otherwise the results of sampling are undefined.
--
-- ObjC selector: @- normalizedCoordinates@
normalizedCoordinates :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
normalizedCoordinates mtlSamplerDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlSamplerDescriptor (mkSelector "normalizedCoordinates") retCULong []

-- | normalizedCoordinates.
--
-- If YES, texture coordates are from 0 to 1.  If NO, texture coordinates are 0..width, 0..height.
--
-- normalizedCoordinates defaults to YES.  Non-normalized coordinates should only be used with 1D and 2D textures with the ClampToEdge wrap mode, otherwise the results of sampling are undefined.
--
-- ObjC selector: @- setNormalizedCoordinates:@
setNormalizedCoordinates :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setNormalizedCoordinates mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setNormalizedCoordinates:") retVoid [argCULong (if value then 1 else 0)]

-- | lodMinClamp
--
-- The minimum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMinClamp is 0.0.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- lodMinClamp@
lodMinClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodMinClamp mtlSamplerDescriptor  =
  sendMsg mtlSamplerDescriptor (mkSelector "lodMinClamp") retCFloat []

-- | lodMinClamp
--
-- The minimum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMinClamp is 0.0.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- setLodMinClamp:@
setLodMinClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodMinClamp mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setLodMinClamp:") retVoid [argCFloat (fromIntegral value)]

-- | lodMaxClamp
--
-- The maximum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMaxClamp is FLT_MAX.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- lodMaxClamp@
lodMaxClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodMaxClamp mtlSamplerDescriptor  =
  sendMsg mtlSamplerDescriptor (mkSelector "lodMaxClamp") retCFloat []

-- | lodMaxClamp
--
-- The maximum level of detail that will be used when sampling from a texture.
--
-- The default value of lodMaxClamp is FLT_MAX.  Clamp values are ignored for texture sample variants that specify an explicit level of detail.
--
-- ObjC selector: @- setLodMaxClamp:@
setLodMaxClamp :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodMaxClamp mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setLodMaxClamp:") retVoid [argCFloat (fromIntegral value)]

-- | lodAverage
--
-- If YES, an average level of detail will be used when sampling from a texture. If NO, no averaging is performed.
--
-- lodAverage defaults to NO. This option is a performance hint. An implementation is free to ignore this property.
--
-- ObjC selector: @- lodAverage@
lodAverage :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
lodAverage mtlSamplerDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlSamplerDescriptor (mkSelector "lodAverage") retCULong []

-- | lodAverage
--
-- If YES, an average level of detail will be used when sampling from a texture. If NO, no averaging is performed.
--
-- lodAverage defaults to NO. This option is a performance hint. An implementation is free to ignore this property.
--
-- ObjC selector: @- setLodAverage:@
setLodAverage :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setLodAverage mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setLodAverage:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the level-of-detail (lod) bias when sampling from a texture.
--
-- The property's default value is @0.0f@. The precision format is @S4.6@, and the range is @[-16.0, 15.999]@.
--
-- ObjC selector: @- lodBias@
lodBias :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO CFloat
lodBias mtlSamplerDescriptor  =
  sendMsg mtlSamplerDescriptor (mkSelector "lodBias") retCFloat []

-- | Sets the level-of-detail (lod) bias when sampling from a texture.
--
-- The property's default value is @0.0f@. The precision format is @S4.6@, and the range is @[-16.0, 15.999]@.
--
-- ObjC selector: @- setLodBias:@
setLodBias :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> CFloat -> IO ()
setLodBias mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setLodBias:") retVoid [argCFloat (fromIntegral value)]

-- | compareFunction
--
-- Set the comparison function used when sampling shadow maps. The default value is MTLCompareFunctionNever.
--
-- ObjC selector: @- compareFunction@
compareFunction :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO MTLCompareFunction
compareFunction mtlSamplerDescriptor  =
  fmap (coerce :: CULong -> MTLCompareFunction) $ sendMsg mtlSamplerDescriptor (mkSelector "compareFunction") retCULong []

-- | compareFunction
--
-- Set the comparison function used when sampling shadow maps. The default value is MTLCompareFunctionNever.
--
-- ObjC selector: @- setCompareFunction:@
setCompareFunction :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> MTLCompareFunction -> IO ()
setCompareFunction mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setCompareFunction:") retVoid [argCULong (coerce value)]

-- | supportArgumentBuffers
--
-- true if the sampler can be used inside an argument buffer
--
-- ObjC selector: @- supportArgumentBuffers@
supportArgumentBuffers :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO Bool
supportArgumentBuffers mtlSamplerDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlSamplerDescriptor (mkSelector "supportArgumentBuffers") retCULong []

-- | supportArgumentBuffers
--
-- true if the sampler can be used inside an argument buffer
--
-- ObjC selector: @- setSupportArgumentBuffers:@
setSupportArgumentBuffers :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> Bool -> IO ()
setSupportArgumentBuffers mtlSamplerDescriptor  value =
  sendMsg mtlSamplerDescriptor (mkSelector "setSupportArgumentBuffers:") retVoid [argCULong (if value then 1 else 0)]

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- label@
label :: IsMTLSamplerDescriptor mtlSamplerDescriptor => mtlSamplerDescriptor -> IO (Id NSString)
label mtlSamplerDescriptor  =
  sendMsg mtlSamplerDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify the created object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLSamplerDescriptor mtlSamplerDescriptor, IsNSString value) => mtlSamplerDescriptor -> value -> IO ()
setLabel mtlSamplerDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlSamplerDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minFilter@
minFilterSelector :: Selector
minFilterSelector = mkSelector "minFilter"

-- | @Selector@ for @setMinFilter:@
setMinFilterSelector :: Selector
setMinFilterSelector = mkSelector "setMinFilter:"

-- | @Selector@ for @magFilter@
magFilterSelector :: Selector
magFilterSelector = mkSelector "magFilter"

-- | @Selector@ for @setMagFilter:@
setMagFilterSelector :: Selector
setMagFilterSelector = mkSelector "setMagFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector
setMipFilterSelector = mkSelector "setMipFilter:"

-- | @Selector@ for @maxAnisotropy@
maxAnisotropySelector :: Selector
maxAnisotropySelector = mkSelector "maxAnisotropy"

-- | @Selector@ for @setMaxAnisotropy:@
setMaxAnisotropySelector :: Selector
setMaxAnisotropySelector = mkSelector "setMaxAnisotropy:"

-- | @Selector@ for @sAddressMode@
sAddressModeSelector :: Selector
sAddressModeSelector = mkSelector "sAddressMode"

-- | @Selector@ for @setSAddressMode:@
setSAddressModeSelector :: Selector
setSAddressModeSelector = mkSelector "setSAddressMode:"

-- | @Selector@ for @tAddressMode@
tAddressModeSelector :: Selector
tAddressModeSelector = mkSelector "tAddressMode"

-- | @Selector@ for @setTAddressMode:@
setTAddressModeSelector :: Selector
setTAddressModeSelector = mkSelector "setTAddressMode:"

-- | @Selector@ for @rAddressMode@
rAddressModeSelector :: Selector
rAddressModeSelector = mkSelector "rAddressMode"

-- | @Selector@ for @setRAddressMode:@
setRAddressModeSelector :: Selector
setRAddressModeSelector = mkSelector "setRAddressMode:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @reductionMode@
reductionModeSelector :: Selector
reductionModeSelector = mkSelector "reductionMode"

-- | @Selector@ for @setReductionMode:@
setReductionModeSelector :: Selector
setReductionModeSelector = mkSelector "setReductionMode:"

-- | @Selector@ for @normalizedCoordinates@
normalizedCoordinatesSelector :: Selector
normalizedCoordinatesSelector = mkSelector "normalizedCoordinates"

-- | @Selector@ for @setNormalizedCoordinates:@
setNormalizedCoordinatesSelector :: Selector
setNormalizedCoordinatesSelector = mkSelector "setNormalizedCoordinates:"

-- | @Selector@ for @lodMinClamp@
lodMinClampSelector :: Selector
lodMinClampSelector = mkSelector "lodMinClamp"

-- | @Selector@ for @setLodMinClamp:@
setLodMinClampSelector :: Selector
setLodMinClampSelector = mkSelector "setLodMinClamp:"

-- | @Selector@ for @lodMaxClamp@
lodMaxClampSelector :: Selector
lodMaxClampSelector = mkSelector "lodMaxClamp"

-- | @Selector@ for @setLodMaxClamp:@
setLodMaxClampSelector :: Selector
setLodMaxClampSelector = mkSelector "setLodMaxClamp:"

-- | @Selector@ for @lodAverage@
lodAverageSelector :: Selector
lodAverageSelector = mkSelector "lodAverage"

-- | @Selector@ for @setLodAverage:@
setLodAverageSelector :: Selector
setLodAverageSelector = mkSelector "setLodAverage:"

-- | @Selector@ for @lodBias@
lodBiasSelector :: Selector
lodBiasSelector = mkSelector "lodBias"

-- | @Selector@ for @setLodBias:@
setLodBiasSelector :: Selector
setLodBiasSelector = mkSelector "setLodBias:"

-- | @Selector@ for @compareFunction@
compareFunctionSelector :: Selector
compareFunctionSelector = mkSelector "compareFunction"

-- | @Selector@ for @setCompareFunction:@
setCompareFunctionSelector :: Selector
setCompareFunctionSelector = mkSelector "setCompareFunction:"

-- | @Selector@ for @supportArgumentBuffers@
supportArgumentBuffersSelector :: Selector
supportArgumentBuffersSelector = mkSelector "supportArgumentBuffers"

-- | @Selector@ for @setSupportArgumentBuffers:@
setSupportArgumentBuffersSelector :: Selector
setSupportArgumentBuffersSelector = mkSelector "setSupportArgumentBuffers:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"


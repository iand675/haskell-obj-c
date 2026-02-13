{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNMaterialProperty
--
-- The contents of a SCNMaterial slot
--
-- This can be used to specify the various properties of SCNMaterial slots such as diffuse, ambient, etc.
--
-- Generated bindings for @SCNMaterialProperty@.
module ObjC.SceneKit.SCNMaterialProperty
  ( SCNMaterialProperty
  , IsSCNMaterialProperty(..)
  , materialPropertyWithContents
  , precomputedLightingEnvironmentContentsWithURL_error
  , precomputedLightingEnvironmentContentsWithData_error
  , precomputedLightingEnvironmentDataForContents_device_error
  , contents
  , setContents
  , intensity
  , setIntensity
  , minificationFilter
  , setMinificationFilter
  , magnificationFilter
  , setMagnificationFilter
  , mipFilter
  , setMipFilter
  , contentsTransform
  , setContentsTransform
  , wrapS
  , setWrapS
  , wrapT
  , setWrapT
  , mappingChannel
  , setMappingChannel
  , textureComponents
  , setTextureComponents
  , maxAnisotropy
  , setMaxAnisotropy
  , borderColor
  , setBorderColor
  , borderColorSelector
  , contentsSelector
  , contentsTransformSelector
  , intensitySelector
  , magnificationFilterSelector
  , mappingChannelSelector
  , materialPropertyWithContentsSelector
  , maxAnisotropySelector
  , minificationFilterSelector
  , mipFilterSelector
  , precomputedLightingEnvironmentContentsWithData_errorSelector
  , precomputedLightingEnvironmentContentsWithURL_errorSelector
  , precomputedLightingEnvironmentDataForContents_device_errorSelector
  , setBorderColorSelector
  , setContentsSelector
  , setContentsTransformSelector
  , setIntensitySelector
  , setMagnificationFilterSelector
  , setMappingChannelSelector
  , setMaxAnisotropySelector
  , setMinificationFilterSelector
  , setMipFilterSelector
  , setTextureComponentsSelector
  , setWrapSSelector
  , setWrapTSelector
  , textureComponentsSelector
  , wrapSSelector
  , wrapTSelector

  -- * Enum types
  , SCNColorMask(SCNColorMask)
  , pattern SCNColorMaskNone
  , pattern SCNColorMaskRed
  , pattern SCNColorMaskGreen
  , pattern SCNColorMaskBlue
  , pattern SCNColorMaskAlpha
  , pattern SCNColorMaskAll
  , SCNFilterMode(SCNFilterMode)
  , pattern SCNFilterModeNone
  , pattern SCNFilterModeNearest
  , pattern SCNFilterModeLinear
  , SCNWrapMode(SCNWrapMode)
  , pattern SCNWrapModeClamp
  , pattern SCNWrapModeRepeat
  , pattern SCNWrapModeClampToBorder
  , pattern SCNWrapModeMirror

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | materialPropertyWithContents:
--
-- Creates and initialize a property instance with the specified contents.
--
-- ObjC selector: @+ materialPropertyWithContents:@
materialPropertyWithContents :: RawId -> IO (Id SCNMaterialProperty)
materialPropertyWithContents contents =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    sendClassMessage cls' materialPropertyWithContentsSelector contents

-- | precomputedLightingEnvironmentContentsWithURL:error:
--
-- Returns an object suitable for a scene's @lightingEnvironment.contents@ and initialized with data that was previously created by @+precomputedLightingEnvironmentDataForContents:device:error:@.
--
-- ObjC selector: @+ precomputedLightingEnvironmentContentsWithURL:error:@
precomputedLightingEnvironmentContentsWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO RawId
precomputedLightingEnvironmentContentsWithURL_error url error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    sendClassMessage cls' precomputedLightingEnvironmentContentsWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | precomputedLightingEnvironmentContentsWithData:error:
--
-- Returns an object suitable for a scene's @lightingEnvironment.contents@ and initialized with data that was previously created by @+precomputedLightingEnvironmentDataForContents:device:error:@.
--
-- ObjC selector: @+ precomputedLightingEnvironmentContentsWithData:error:@
precomputedLightingEnvironmentContentsWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
precomputedLightingEnvironmentContentsWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    sendClassMessage cls' precomputedLightingEnvironmentContentsWithData_errorSelector (toNSData data_) (toNSError error_)

-- | precomputedLightingEnvironmentDataForContents:device:error:
--
-- Returns an @NSData@ instance containing the result of CPU and GPU-intensive operations that is suitable for caching.
--
-- This method can be leveraged in a custom offline asset pipeline, or at run time at a convenient time before the scene is presented to the user.
--
-- ObjC selector: @+ precomputedLightingEnvironmentDataForContents:device:error:@
precomputedLightingEnvironmentDataForContents_device_error :: IsNSError error_ => RawId -> RawId -> error_ -> IO (Id NSData)
precomputedLightingEnvironmentDataForContents_device_error contents device error_ =
  do
    cls' <- getRequiredClass "SCNMaterialProperty"
    sendClassMessage cls' precomputedLightingEnvironmentDataForContents_device_errorSelector contents device (toNSError error_)

-- | contents
--
-- Specifies the receiver's contents. This can be a color (NSColor, UIColor, CGColorRef), an image (NSImage, UIImage, CGImageRef), a layer (CALayer), a path (NSString or NSURL), a SpriteKit scene (SKScene), a texture (SKTexture, id<MTLTexture> or GLKTextureInfo), or a floating value between 0 and 1 (NSNumber) for metalness and roughness properties. AVCaptureDevice is supported on iOS 11 and AVPlayer is supported on macOS 10.13, iOS 11 and tvOS 11. Animatable when set to a color.
--
-- Setting the contents to an instance of SKTexture will automatically update the wrapS, wrapT, contentsTransform, minification, magnification and mip filters according to the SKTexture settings.             When a cube map is expected (e.g. SCNMaterial.reflective, SCNScene.background, SCNScene.lightingEnvironment) you can use               1. A horizontal strip image                          where @6 * image.height ==     image.width@               2. A vertical strip image                            where @    image.height == 6 * image.width@               3. A spherical projection image (latitude/longitude) where @2 * image.height ==     image.width@               4. A NSArray of 6 images. This array must contain images of the exact same dimensions, in the following order, in a left-handed coordinate system: +X, -X, +Y, -Y, +Z, -Z (or Right, Left, Top, Bottom, Front, Back).
--
-- ObjC selector: @- contents@
contents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO RawId
contents scnMaterialProperty =
  sendMessage scnMaterialProperty contentsSelector

-- | contents
--
-- Specifies the receiver's contents. This can be a color (NSColor, UIColor, CGColorRef), an image (NSImage, UIImage, CGImageRef), a layer (CALayer), a path (NSString or NSURL), a SpriteKit scene (SKScene), a texture (SKTexture, id<MTLTexture> or GLKTextureInfo), or a floating value between 0 and 1 (NSNumber) for metalness and roughness properties. AVCaptureDevice is supported on iOS 11 and AVPlayer is supported on macOS 10.13, iOS 11 and tvOS 11. Animatable when set to a color.
--
-- Setting the contents to an instance of SKTexture will automatically update the wrapS, wrapT, contentsTransform, minification, magnification and mip filters according to the SKTexture settings.             When a cube map is expected (e.g. SCNMaterial.reflective, SCNScene.background, SCNScene.lightingEnvironment) you can use               1. A horizontal strip image                          where @6 * image.height ==     image.width@               2. A vertical strip image                            where @    image.height == 6 * image.width@               3. A spherical projection image (latitude/longitude) where @2 * image.height ==     image.width@               4. A NSArray of 6 images. This array must contain images of the exact same dimensions, in the following order, in a left-handed coordinate system: +X, -X, +Y, -Y, +Z, -Z (or Right, Left, Top, Bottom, Front, Back).
--
-- ObjC selector: @- setContents:@
setContents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> RawId -> IO ()
setContents scnMaterialProperty value =
  sendMessage scnMaterialProperty setContentsSelector value

-- | intensity
--
-- Determines the receiver's intensity. This intensity is used to modulate the properties in several ways. It dims the diffuse, specular and emission properties, it varies the bumpiness of the normal property and the filter property is blended with white. Default value is 1.0. Animatable.
--
-- ObjC selector: @- intensity@
intensity :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CDouble
intensity scnMaterialProperty =
  sendMessage scnMaterialProperty intensitySelector

-- | intensity
--
-- Determines the receiver's intensity. This intensity is used to modulate the properties in several ways. It dims the diffuse, specular and emission properties, it varies the bumpiness of the normal property and the filter property is blended with white. Default value is 1.0. Animatable.
--
-- ObjC selector: @- setIntensity:@
setIntensity :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CDouble -> IO ()
setIntensity scnMaterialProperty value =
  sendMessage scnMaterialProperty setIntensitySelector value

-- | minificationFilter
--
-- Specifies the filter type to use when rendering the contents (specified in the `contents' property).
--
-- The minification filter is used when to reduce the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- minificationFilter@
minificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
minificationFilter scnMaterialProperty =
  sendMessage scnMaterialProperty minificationFilterSelector

-- | minificationFilter
--
-- Specifies the filter type to use when rendering the contents (specified in the `contents' property).
--
-- The minification filter is used when to reduce the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- setMinificationFilter:@
setMinificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMinificationFilter scnMaterialProperty value =
  sendMessage scnMaterialProperty setMinificationFilterSelector value

-- | magnificationFilter
--
-- Specifies the filter type to use when rendering the the contents (specified in the `contents' property).
--
-- The magnification filter is used when to increase the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- magnificationFilter@
magnificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
magnificationFilter scnMaterialProperty =
  sendMessage scnMaterialProperty magnificationFilterSelector

-- | magnificationFilter
--
-- Specifies the filter type to use when rendering the the contents (specified in the `contents' property).
--
-- The magnification filter is used when to increase the size of image data. See above the list of available modes. Defaults to SCNFilterModeLinear.
--
-- ObjC selector: @- setMagnificationFilter:@
setMagnificationFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMagnificationFilter scnMaterialProperty value =
  sendMessage scnMaterialProperty setMagnificationFilterSelector value

-- | mipFilter
--
-- Specifies the mipmap filter to use during minification.
--
-- Defaults to SCNFilterModeNearest starting macOS 10.12, iOS 10, tvOS 10 and watchOS 3. Defaults to SCNFilterModeNone in previous versions.
--
-- ObjC selector: @- mipFilter@
mipFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNFilterMode
mipFilter scnMaterialProperty =
  sendMessage scnMaterialProperty mipFilterSelector

-- | mipFilter
--
-- Specifies the mipmap filter to use during minification.
--
-- Defaults to SCNFilterModeNearest starting macOS 10.12, iOS 10, tvOS 10 and watchOS 3. Defaults to SCNFilterModeNone in previous versions.
--
-- ObjC selector: @- setMipFilter:@
setMipFilter :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNFilterMode -> IO ()
setMipFilter scnMaterialProperty value =
  sendMessage scnMaterialProperty setMipFilterSelector value

-- | contentsTransform
--
-- Determines the receiver's contents transform. Animatable.
--
-- ObjC selector: @- contentsTransform@
contentsTransform :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNMatrix4
contentsTransform scnMaterialProperty =
  sendMessage scnMaterialProperty contentsTransformSelector

-- | contentsTransform
--
-- Determines the receiver's contents transform. Animatable.
--
-- ObjC selector: @- setContentsTransform:@
setContentsTransform :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNMatrix4 -> IO ()
setContentsTransform scnMaterialProperty value =
  sendMessage scnMaterialProperty setContentsTransformSelector value

-- | wrapS
--
-- Determines the receiver's wrap mode for the s texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- wrapS@
wrapS :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNWrapMode
wrapS scnMaterialProperty =
  sendMessage scnMaterialProperty wrapSSelector

-- | wrapS
--
-- Determines the receiver's wrap mode for the s texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- setWrapS:@
setWrapS :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNWrapMode -> IO ()
setWrapS scnMaterialProperty value =
  sendMessage scnMaterialProperty setWrapSSelector value

-- | wrapT
--
-- Determines the receiver's wrap mode for the t texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- wrapT@
wrapT :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNWrapMode
wrapT scnMaterialProperty =
  sendMessage scnMaterialProperty wrapTSelector

-- | wrapT
--
-- Determines the receiver's wrap mode for the t texture coordinate. Defaults to SCNWrapModeClamp.
--
-- ObjC selector: @- setWrapT:@
setWrapT :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNWrapMode -> IO ()
setWrapT scnMaterialProperty value =
  sendMessage scnMaterialProperty setWrapTSelector value

-- | mappingChannel
--
-- Determines the receiver's mapping channel. Defaults to 0.
--
-- Geometries potentially have multiple sources of texture coordinates. Every source has a unique mapping channel index. The mapping channel allows to select which source of texture coordinates is used to map the content of the receiver.
--
-- ObjC selector: @- mappingChannel@
mappingChannel :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CLong
mappingChannel scnMaterialProperty =
  sendMessage scnMaterialProperty mappingChannelSelector

-- | mappingChannel
--
-- Determines the receiver's mapping channel. Defaults to 0.
--
-- Geometries potentially have multiple sources of texture coordinates. Every source has a unique mapping channel index. The mapping channel allows to select which source of texture coordinates is used to map the content of the receiver.
--
-- ObjC selector: @- setMappingChannel:@
setMappingChannel :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CLong -> IO ()
setMappingChannel scnMaterialProperty value =
  sendMessage scnMaterialProperty setMappingChannelSelector value

-- | textureComponents
--
-- Specifies the texture components to sample in the shader. Defaults to SCNColorMaskRed for displacement property, and to SCNColorMaskAll for other properties.
--
-- Use this property to when using a texture that combine multiple informations in the different texture components. For example if you pack the roughness in red and metalness in blue etc... You can specify what component to use from the texture for this given material property. This property is only supported by Metal renderers.
--
-- ObjC selector: @- textureComponents@
textureComponents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO SCNColorMask
textureComponents scnMaterialProperty =
  sendMessage scnMaterialProperty textureComponentsSelector

-- | textureComponents
--
-- Specifies the texture components to sample in the shader. Defaults to SCNColorMaskRed for displacement property, and to SCNColorMaskAll for other properties.
--
-- Use this property to when using a texture that combine multiple informations in the different texture components. For example if you pack the roughness in red and metalness in blue etc... You can specify what component to use from the texture for this given material property. This property is only supported by Metal renderers.
--
-- ObjC selector: @- setTextureComponents:@
setTextureComponents :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> SCNColorMask -> IO ()
setTextureComponents scnMaterialProperty value =
  sendMessage scnMaterialProperty setTextureComponentsSelector value

-- | maxAnisotropy
--
-- Specifies the receiver's max anisotropy. Defaults to MAXFLOAT.
--
-- Anisotropic filtering reduces blur and preserves detail at extreme viewing angles.
--
-- ObjC selector: @- maxAnisotropy@
maxAnisotropy :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO CDouble
maxAnisotropy scnMaterialProperty =
  sendMessage scnMaterialProperty maxAnisotropySelector

-- | maxAnisotropy
--
-- Specifies the receiver's max anisotropy. Defaults to MAXFLOAT.
--
-- Anisotropic filtering reduces blur and preserves detail at extreme viewing angles.
--
-- ObjC selector: @- setMaxAnisotropy:@
setMaxAnisotropy :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> CDouble -> IO ()
setMaxAnisotropy scnMaterialProperty value =
  sendMessage scnMaterialProperty setMaxAnisotropySelector value

-- | borderColor
--
-- Determines the receiver's border color (CGColorRef or NSColor). Animatable.
--
-- The border color is ignored on iOS and is always considered as clear color (0,0,0,0) when the texture has an alpha channel and opaque back (0,0,0,1) otherwise.
--
-- ObjC selector: @- borderColor@
borderColor :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> IO RawId
borderColor scnMaterialProperty =
  sendMessage scnMaterialProperty borderColorSelector

-- | borderColor
--
-- Determines the receiver's border color (CGColorRef or NSColor). Animatable.
--
-- The border color is ignored on iOS and is always considered as clear color (0,0,0,0) when the texture has an alpha channel and opaque back (0,0,0,1) otherwise.
--
-- ObjC selector: @- setBorderColor:@
setBorderColor :: IsSCNMaterialProperty scnMaterialProperty => scnMaterialProperty -> RawId -> IO ()
setBorderColor scnMaterialProperty value =
  sendMessage scnMaterialProperty setBorderColorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @materialPropertyWithContents:@
materialPropertyWithContentsSelector :: Selector '[RawId] (Id SCNMaterialProperty)
materialPropertyWithContentsSelector = mkSelector "materialPropertyWithContents:"

-- | @Selector@ for @precomputedLightingEnvironmentContentsWithURL:error:@
precomputedLightingEnvironmentContentsWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] RawId
precomputedLightingEnvironmentContentsWithURL_errorSelector = mkSelector "precomputedLightingEnvironmentContentsWithURL:error:"

-- | @Selector@ for @precomputedLightingEnvironmentContentsWithData:error:@
precomputedLightingEnvironmentContentsWithData_errorSelector :: Selector '[Id NSData, Id NSError] RawId
precomputedLightingEnvironmentContentsWithData_errorSelector = mkSelector "precomputedLightingEnvironmentContentsWithData:error:"

-- | @Selector@ for @precomputedLightingEnvironmentDataForContents:device:error:@
precomputedLightingEnvironmentDataForContents_device_errorSelector :: Selector '[RawId, RawId, Id NSError] (Id NSData)
precomputedLightingEnvironmentDataForContents_device_errorSelector = mkSelector "precomputedLightingEnvironmentDataForContents:device:error:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] RawId
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[RawId] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @intensity@
intensitySelector :: Selector '[] CDouble
intensitySelector = mkSelector "intensity"

-- | @Selector@ for @setIntensity:@
setIntensitySelector :: Selector '[CDouble] ()
setIntensitySelector = mkSelector "setIntensity:"

-- | @Selector@ for @minificationFilter@
minificationFilterSelector :: Selector '[] SCNFilterMode
minificationFilterSelector = mkSelector "minificationFilter"

-- | @Selector@ for @setMinificationFilter:@
setMinificationFilterSelector :: Selector '[SCNFilterMode] ()
setMinificationFilterSelector = mkSelector "setMinificationFilter:"

-- | @Selector@ for @magnificationFilter@
magnificationFilterSelector :: Selector '[] SCNFilterMode
magnificationFilterSelector = mkSelector "magnificationFilter"

-- | @Selector@ for @setMagnificationFilter:@
setMagnificationFilterSelector :: Selector '[SCNFilterMode] ()
setMagnificationFilterSelector = mkSelector "setMagnificationFilter:"

-- | @Selector@ for @mipFilter@
mipFilterSelector :: Selector '[] SCNFilterMode
mipFilterSelector = mkSelector "mipFilter"

-- | @Selector@ for @setMipFilter:@
setMipFilterSelector :: Selector '[SCNFilterMode] ()
setMipFilterSelector = mkSelector "setMipFilter:"

-- | @Selector@ for @contentsTransform@
contentsTransformSelector :: Selector '[] SCNMatrix4
contentsTransformSelector = mkSelector "contentsTransform"

-- | @Selector@ for @setContentsTransform:@
setContentsTransformSelector :: Selector '[SCNMatrix4] ()
setContentsTransformSelector = mkSelector "setContentsTransform:"

-- | @Selector@ for @wrapS@
wrapSSelector :: Selector '[] SCNWrapMode
wrapSSelector = mkSelector "wrapS"

-- | @Selector@ for @setWrapS:@
setWrapSSelector :: Selector '[SCNWrapMode] ()
setWrapSSelector = mkSelector "setWrapS:"

-- | @Selector@ for @wrapT@
wrapTSelector :: Selector '[] SCNWrapMode
wrapTSelector = mkSelector "wrapT"

-- | @Selector@ for @setWrapT:@
setWrapTSelector :: Selector '[SCNWrapMode] ()
setWrapTSelector = mkSelector "setWrapT:"

-- | @Selector@ for @mappingChannel@
mappingChannelSelector :: Selector '[] CLong
mappingChannelSelector = mkSelector "mappingChannel"

-- | @Selector@ for @setMappingChannel:@
setMappingChannelSelector :: Selector '[CLong] ()
setMappingChannelSelector = mkSelector "setMappingChannel:"

-- | @Selector@ for @textureComponents@
textureComponentsSelector :: Selector '[] SCNColorMask
textureComponentsSelector = mkSelector "textureComponents"

-- | @Selector@ for @setTextureComponents:@
setTextureComponentsSelector :: Selector '[SCNColorMask] ()
setTextureComponentsSelector = mkSelector "setTextureComponents:"

-- | @Selector@ for @maxAnisotropy@
maxAnisotropySelector :: Selector '[] CDouble
maxAnisotropySelector = mkSelector "maxAnisotropy"

-- | @Selector@ for @setMaxAnisotropy:@
setMaxAnisotropySelector :: Selector '[CDouble] ()
setMaxAnisotropySelector = mkSelector "setMaxAnisotropy:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] RawId
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[RawId] ()
setBorderColorSelector = mkSelector "setBorderColor:"

